{-# LANGUAGE CPP #-}
{-|
hledger-vty - a hledger add-on providing a curses-style interface.
Copyright (c) 2007-2011 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.
-}

module Hledger.Vty.Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Graphics.Vty
import Safe (headDef)
import System.Console.GetOpt

import Hledger
import Prelude hiding (putStr, putStrLn)
import Hledger.Utils.UTF8 (putStr, putStrLn)
import Hledger.Cli


progname_vty = progname_cli ++ "-vty"

options_vty :: [OptDescr Opt]
options_vty = [
 Option ""  ["debug-vty"]    (NoArg  DebugVty)      "run with no terminal output, showing console"
 ]

usage_preamble_vty =
  "Usage: hledger-vty [OPTIONS] [PATTERNS]\n" ++
  "\n" ++
  "Reads your ~/.journal file, or another specified by $LEDGER or -f, and\n" ++
  "starts the full-window curses ui.\n" ++
  "\n"

usage_options_vty = usageInfo "hledger-vty options:" options_vty ++ "\n"

usage_vty = concat [
             usage_preamble_vty
            ,usage_options_vty
            ,usage_options_cli
            ,usage_postscript_cli
            ]

main :: IO ()
main = do
  (opts, args) <- parseArgumentsWith $ options_cli++options_vty
  run opts args
    where
      run opts args
       | Help `elem` opts             = putStr usage_vty
       | Version `elem` opts          = putStrLn $ progversionstr progname_vty
       | BinaryFilename `elem` opts   = putStrLn $ binaryfilename progname_vty
       | otherwise                    = withJournalDo opts args "vty" vty

helpmsg = "(b)alance, (r)egister, (p)rint, (right) to drill down, (left) to back up, (q)uit"

instance Show Vty where show = const "a Vty"

-- | The application state when running the vty command.
data AppState = AppState {
     av :: Vty                   -- ^ the vty context
    ,aw :: Int                  -- ^ window width
    ,ah :: Int                  -- ^ window height
    ,amsg :: String              -- ^ status message
    ,aopts :: [Opt]              -- ^ command-line opts
    ,aargs :: [String]           -- ^ command-line args at startup
    ,ajournal :: Journal         -- ^ parsed journal
    ,abuf :: [String]            -- ^ lines of the current buffered view
    ,alocs :: [Loc]              -- ^ user's navigation trail within the UI
                                -- ^ never null, head is current location
    } deriving (Show)

-- | A location within the user interface.
data Loc = Loc {
     scr :: Screen               -- ^ one of the available screens
    ,sy :: Int                   -- ^ viewport y scroll position
    ,cy :: Int                   -- ^ cursor y position
    ,largs :: [String]           -- ^ command-line args, possibly narrowed for this location
    } deriving (Show)

-- | The screens available within the user interface.
data Screen = BalanceScreen     -- ^ like hledger balance, shows accounts
            | RegisterScreen    -- ^ like hledger register, shows transaction-postings
            | PrintScreen       -- ^ like hledger print, shows journal transactions
            -- | LedgerScreen      -- ^ shows the raw journal entries
              deriving (Eq,Show)

-- | Run the vty (curses-style) ui.
vty :: [Opt] -> [String] -> Journal -> IO ()
vty opts args j = do
  v <- mkVty
  DisplayRegion w h <- display_bounds $ terminal v
  d <-  getCurrentDay
  let a = enter d BalanceScreen args
          AppState {
                  av=v
                 ,aw=fromIntegral w
                 ,ah=fromIntegral h
                 ,amsg=helpmsg
                 ,aopts=opts
                 ,aargs=args
                 ,ajournal=j
                 ,abuf=[]
                 ,alocs=[]
                 }
  go a 

-- | Update the screen, wait for the next event, repeat.
go :: AppState -> IO ()
go a@AppState{av=av,aopts=opts} = do
  when (notElem DebugVty opts) $ update av (renderScreen a)
  k <- next_event av
  d <- getCurrentDay
  case k of 
    EvResize x y                -> go $ resize x y a
    EvKey (KASCII 'l') [MCtrl]  -> refresh av >> go a{amsg=helpmsg}
    EvKey (KASCII 'b') []       -> go $ resetTrailAndEnter d BalanceScreen a
    EvKey (KASCII 'r') []       -> go $ resetTrailAndEnter d RegisterScreen a
    EvKey (KASCII 'p') []       -> go $ resetTrailAndEnter d PrintScreen a
    EvKey KRight []             -> go $ drilldown d a
    EvKey KEnter []             -> go $ drilldown d a
    EvKey KLeft  []             -> go $ backout d a
    EvKey KUp    []             -> go $ moveUpAndPushEdge a
    EvKey KDown  []             -> go $ moveDownAndPushEdge a
    EvKey KHome  []             -> go $ moveToTop a
    EvKey KUp    [MCtrl]        -> go $ moveToTop a
    EvKey KUp    [MShift]       -> go $ moveToTop a
    EvKey KEnd   []             -> go $ moveToBottom a
    EvKey KDown  [MCtrl]        -> go $ moveToBottom a
    EvKey KDown  [MShift]       -> go $ moveToBottom a
    EvKey KPageUp []            -> go $ prevpage a
    EvKey KBS []                -> go $ prevpage a
    EvKey (KASCII ' ') [MShift] -> go $ prevpage a
    EvKey KPageDown []          -> go $ nextpage a
    EvKey (KASCII ' ') []       -> go $ nextpage a
    EvKey (KASCII 'q') []       -> shutdown av >> return ()
--    EvKey KEsc   []           -> shutdown av >> return ()
    _                           -> go a

-- app state modifiers

-- | The number of lines currently available for the main data display area.
pageHeight :: AppState -> Int
pageHeight a = ah a - 1

setLocCursorY, setLocScrollY :: Int -> Loc -> Loc
setLocCursorY y l = l{cy=y}
setLocScrollY y l = l{sy=y}

cursorY, scrollY, posY :: AppState -> Int
cursorY = cy . loc
scrollY = sy . loc
posY a = scrollY a + cursorY a

setCursorY, setScrollY, setPosY :: Int -> AppState -> AppState
setCursorY _ AppState{alocs=[]} = error' "shouldn't happen" -- silence warnings
setCursorY y a@AppState{alocs=(l:locs)} = a{alocs=(l':locs)} where l' = setLocCursorY y l

setScrollY _ AppState{alocs=[]} = error' "shouldn't happen" -- silence warnings
setScrollY y a@AppState{alocs=(l:locs)} = a{alocs=(l':locs)} where l' = setLocScrollY y l

setPosY _ AppState{alocs=[]}    = error' "shouldn't happen" -- silence warnings
setPosY y a@AppState{alocs=(l:locs)} = a{alocs=(l':locs)}
    where 
      l' = setLocScrollY sy $ setLocCursorY cy l
      ph = pageHeight a
      cy = y `mod` ph
      sy = y - cy

updateCursorY, updateScrollY, updatePosY :: (Int -> Int) -> AppState -> AppState
updateCursorY f a = setCursorY (f $ cursorY a) a
updateScrollY f a = setScrollY (f $ scrollY a) a
updatePosY f a = setPosY (f $ posY a) a

resize :: Int -> Int -> AppState -> AppState
resize x y a = setCursorY cy' a{aw=x,ah=y}
    where
      cy = cursorY a
      cy' = min cy (y-2)

moveToTop :: AppState -> AppState
moveToTop = setPosY 0

moveToBottom :: AppState -> AppState
moveToBottom a = setPosY (length $ abuf a) a

moveUpAndPushEdge :: AppState -> AppState
moveUpAndPushEdge a
    | cy > 0 = updateCursorY (subtract 1) a
    | sy > 0 = updateScrollY (subtract 1) a
    | otherwise = a
    where Loc{sy=sy,cy=cy} = head $ alocs a

moveDownAndPushEdge :: AppState -> AppState
moveDownAndPushEdge a
    | sy+cy >= bh = a
    | cy < ph-1 = updateCursorY (+1) a
    | otherwise = updateScrollY (+1) a
    where 
      Loc{sy=sy,cy=cy} = head $ alocs a
      ph = pageHeight a
      bh = length $ abuf a

-- | Scroll down by page height or until we can just see the last line,
-- without moving the cursor, or if we are already scrolled as far as
-- possible then move the cursor to the last line.
nextpage :: AppState -> AppState
nextpage (a@AppState{abuf=b})
    | sy < bh-jump = setScrollY sy' a
    | otherwise    = setCursorY (bh-sy) a
    where
      sy = scrollY a
      jump = pageHeight a - 1
      bh = length b
      sy' = min (sy+jump) (bh-jump)

-- | Scroll up by page height or until we can just see the first line,
-- without moving the cursor, or if we are scrolled as far as possible
-- then move the cursor to the first line.
prevpage :: AppState -> AppState
prevpage a
    | sy > 0    = setScrollY sy' a
    | otherwise = setCursorY 0 a
    where
      sy = scrollY a
      jump = pageHeight a - 1
      sy' = max (sy-jump) 0

-- | Push a new UI location on to the stack.
pushLoc :: Loc -> AppState -> AppState
pushLoc l a = a{alocs=(l:alocs a)}

popLoc :: AppState -> AppState
popLoc a@AppState{alocs=locs}
    | length locs > 1 = a{alocs=drop 1 locs}
    | otherwise = a

clearLocs :: AppState -> AppState
clearLocs a = a{alocs=[]}

exit :: AppState -> AppState 
exit = popLoc

loc :: AppState -> Loc
loc = head . alocs

-- | Get the filter pattern args in effect for the current ui location.
currentArgs :: AppState -> [String]
currentArgs (AppState {alocs=Loc{largs=as}:_}) = as
currentArgs (AppState {aargs=as}) = as

screen :: AppState -> Screen
screen a = scr where (Loc scr _ _ _) = loc a

-- | Enter a new screen, with possibly new args, adding the new ui location to the stack.
enter :: Day -> Screen -> [String] -> AppState -> AppState
enter d scr@BalanceScreen args a  = updateData d $ pushLoc Loc{scr=scr,sy=0,cy=0,largs=args} a
enter d scr@RegisterScreen args a = updateData d $ pushLoc Loc{scr=scr,sy=0,cy=0,largs=args} a
enter d scr@PrintScreen args a    = updateData d $ pushLoc Loc{scr=scr,sy=0,cy=0,largs=args} a

resetTrailAndEnter :: Day -> Screen -> AppState -> AppState
resetTrailAndEnter d scr a = enter d scr (aargs a) $ clearLocs a

-- | Regenerate the display data appropriate for the current screen.
updateData :: Day -> AppState -> AppState
updateData d a@AppState{aopts=opts,ajournal=j} =
    case screen a of
      BalanceScreen  -> a{abuf=accountsReportAsText opts $ accountsReport opts fspec j}
      RegisterScreen -> a{abuf=lines $ postingsReportAsText opts $ postingsReport opts fspec j}
      PrintScreen    -> a{abuf=lines $ showTransactions opts fspec j}
    where fspec = optsToFilterSpec opts (currentArgs a) d

backout :: Day -> AppState -> AppState
backout d a | screen a == BalanceScreen = a
            | otherwise = updateData d $ popLoc a

drilldown :: Day -> AppState -> AppState
drilldown d a =
    case screen a of
      BalanceScreen  -> enter d RegisterScreen [currentAccountName a] a
      RegisterScreen -> scrollToTransaction e $ enter d PrintScreen (currentArgs a) a
      PrintScreen   -> a
    where e = currentTransaction a

-- | Get the account name currently highlighted by the cursor on the
-- balance screen. Results undefined while on other screens.
currentAccountName :: AppState -> AccountName
currentAccountName a = accountNameAt (abuf a) (posY a)

-- | Get the full name of the account being displayed at a specific line
-- within the balance command's output.
accountNameAt :: [String] -> Int -> AccountName
accountNameAt buf lineno = accountNameFromComponents anamecomponents
    where
      namestohere = map (drop 22) $ take (lineno+1) buf
      (indented, nonindented) = span (" " `isPrefixOf`) $ reverse namestohere
      thisbranch = indented ++ take 1 nonindented
      anamecomponents = reverse $ map strip $ dropsiblings thisbranch
      dropsiblings :: [AccountName] -> [AccountName]
      dropsiblings [] = []
      dropsiblings (x:xs) = x : dropsiblings xs'
          where
            xs' = dropWhile moreindented xs
            moreindented = (>= myindent) . indentof
            myindent = indentof x
            indentof = length . takeWhile (==' ')

-- | If on the print screen, move the cursor to highlight the specified entry
-- (or a reasonable guess). Doesn't work.
scrollToTransaction :: Maybe Transaction -> AppState -> AppState
scrollToTransaction Nothing a = a
scrollToTransaction (Just t) a@AppState{abuf=buf} = setCursorY cy $ setScrollY sy a
    where
      entryfirstline = head $ lines $ showTransaction t
      halfph = pageHeight a `div` 2
      y = fromMaybe 0 $ findIndex (== entryfirstline) buf
      sy = max 0 $ y - halfph
      cy = y - sy

-- | Get the transaction containing the posting currently highlighted by
-- the cursor on the register screen (or best guess). Results undefined
-- while on other screens.
currentTransaction :: AppState -> Maybe Transaction
currentTransaction a@AppState{ajournal=j,abuf=buf} = ptransaction p
    where
      p = headDef nullposting $ filter ismatch $ journalPostings j
      ismatch p = postingDate p == parsedate (take 10 datedesc)
                  && take 70 (showPostingWithBalanceForVty False p nullmixedamt) == (datedesc ++ acctamt)
      datedesc = take 32 $ fromMaybe "" $ find (not . (" " `isPrefixOf`)) $ headDef "" rest : reverse above
      acctamt = drop 32 $ headDef "" rest
      (above,rest) = splitAt y buf
      y = posY a

-- renderers

renderScreen :: AppState -> Picture
renderScreen (a@AppState{aw=w,ah=h,abuf=buf,amsg=msg}) =
    Picture {pic_cursor = Cursor (fromIntegral cx) (fromIntegral cy)
            ,pic_image = mainimg
                         <->
                         renderStatus w msg
            ,pic_background = Background ' ' def_attr
            }
    where 
      (cx, cy) = (0, cursorY a)
      sy = scrollY a
--       mainimg = (renderString attr $ unlines $ above)
--           <->
--           (renderString reverseattr $ thisline)
--           <->
--           (renderString attr $ unlines $ below)
--       (above,(thisline:below))
--           | null ls   = ([],[""])
--           | otherwise = splitAt y ls
--       ls = lines $ fitto w (h-1) $ unlines $ drop as $ buf
-- trying for more speed
      mainimg = vert_cat (map (string defaultattr) above)
               <->
               string currentlineattr thisline
               <->
               vert_cat (map (string defaultattr) below)
      (thisline,below) | null rest = (blankline,[])
                       | otherwise = (head rest, tail rest)
      (above,rest) = splitAt cy linestorender
      linestorender = map padclipline $ take (h-1) $ drop sy $ buf ++ replicate h blankline
      padclipline = take w . (++ blankline)
      blankline = replicate w ' '

padClipString :: Int -> Int -> String -> [String]
padClipString h w s = rows
    where
      rows = map padclipline $ take h $ lines s ++ replicate h blankline
      padclipline = take w . (++ blankline)
      blankline = replicate w ' '

renderString :: Attr -> String -> Image
renderString attr s = vert_cat $ map (string attr) rows
    where
      rows = lines $ fitto w h s
      w = maximum $ map length ls
      h = length ls
      ls = lines s

renderStatus :: Int -> String -> Image
renderStatus w = string statusattr . take w . (++ repeat ' ')

-- the all-important theming engine!

theme = Restrained

data UITheme = Restrained | Colorful | Blood

(defaultattr, 
 currentlineattr, 
 statusattr
 ) = case theme of
       Restrained -> (def_attr
                    ,def_attr `with_style` bold
                    ,def_attr `with_style` reverse_video
                    )
       Colorful   -> (def_attr `with_style` reverse_video
                    ,def_attr `with_fore_color` white `with_back_color` red
                    ,def_attr `with_fore_color` black `with_back_color` green
                    )
       Blood      -> (def_attr `with_style` reverse_video
                    ,def_attr `with_fore_color` white `with_back_color` red
                    ,def_attr `with_style` reverse_video
                    )

halfbrightattr = def_attr `with_style` dim
reverseattr = def_attr `with_style` reverse_video
redattr = def_attr `with_fore_color` red
greenattr = def_attr `with_fore_color` green
reverseredattr = def_attr `with_style` reverse_video `with_fore_color` red
reversegreenattr= def_attr `with_style` reverse_video `with_fore_color` green
