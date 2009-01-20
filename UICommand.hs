{-| 

A simple text UI for hledger, based on the vty library.

-}

module UICommand
where
import qualified Data.Map as Map
import Data.Map ((!))
import Graphics.Vty
import qualified Data.ByteString.Char8 as B
import Ledger
import Options
import BalanceCommand
import RegisterCommand
import PrintCommand


helpmsg = "Welcome to hledger vty ui. (b)alances, (r)egister, (p)rint entries, (l)edger, (right) to drill down, (left) to back up, or (q)uit"

instance Show Vty where show v = "a Vty"

-- | The application state when running the ui command.
data AppState = AppState {
     av :: Vty                   -- ^ the vty context
    ,aw :: Int                   -- ^ window width
    ,ah :: Int                   -- ^ window height
    ,amsg :: String              -- ^ status message
    ,aopts :: [Opt]              -- ^ command-line opts
    ,aargs :: [String]           -- ^ command-line args
    ,aledger :: Ledger           -- ^ parsed ledger
    ,abuf :: [String]            -- ^ lines of the current buffered view
    ,alocs :: [Loc]              -- ^ user's navigation trail within the UI
                                -- ^ never null, head is current location
    } deriving (Show)

-- | A location within the user interface.
data Loc = Loc {
     scr :: Screen               -- ^ one of the available screens
    ,sy :: Int                   -- ^ viewport y scroll position
    ,cy :: Int                   -- ^ cursor y position
    } deriving (Show)

-- | The screens available within the user interface.
data Screen = BalanceScreen     -- ^ like hledger balance, shows accounts
            | RegisterScreen    -- ^ like hledger register, shows transactions
            | PrintScreen       -- ^ like hledger print, shows entries
            | LedgerScreen      -- ^ shows the raw ledger
              deriving (Eq,Show)

-- | Run the interactive text ui.
ui :: [Opt] -> [String] -> Ledger -> IO ()
ui opts args l = do
  v <- mkVty
  (w,h) <- getSize v
  let opts' = SubTotal:opts
  let a = enter BalanceScreen $ 
          AppState {
                  av=v
                 ,aw=w
                 ,ah=h
                 ,amsg=helpmsg
                 ,aopts=opts'
                 ,aargs=args
                 ,aledger=l
                 ,abuf=[]
                 ,alocs=[]
                 }
  go a 

-- | Update the screen, wait for the next event, repeat.
go :: AppState -> IO ()
go a@AppState{av=av,aw=aw,ah=ah,abuf=buf,amsg=amsg,aopts=opts,aargs=args,aledger=l} = do
  when (not $ DebugNoUI `elem` opts) $ update av (renderScreen a)
  k <- getEvent av
  case k of 
    EvResize x y                -> go $ resize x y a
    EvKey (KASCII 'l') [MCtrl]  -> refresh av >> go a{amsg=helpmsg}
    EvKey (KASCII 'b') []       -> go $ resetTrailAndEnter BalanceScreen a
    EvKey (KASCII 'r') []       -> go $ resetTrailAndEnter RegisterScreen a
    EvKey (KASCII 'p') []       -> go $ resetTrailAndEnter PrintScreen a
    EvKey (KASCII 'l') []       -> go $ resetTrailAndEnter LedgerScreen a
    EvKey KRight []             -> go $ drilldown a
    EvKey KEnter []             -> go $ drilldown a
    EvKey KLeft  []             -> go $ backout a
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
    where
      bh = length buf
      y = posY a

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
setCursorY y a@AppState{alocs=(l:locs)} = a{alocs=(l':locs)} where l' = setLocCursorY y l
setScrollY y a@AppState{alocs=(l:locs)} = a{alocs=(l':locs)} where l' = setLocScrollY y l
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
moveToTop a = setPosY 0 a

moveToBottom :: AppState -> AppState
moveToBottom a = setPosY (length $ abuf a) a

moveUpAndPushEdge :: AppState -> AppState
moveUpAndPushEdge a@AppState{alocs=(Loc{sy=sy,cy=cy}:_)}
    | cy > 0 = updateCursorY (subtract 1) a
    | sy > 0 = updateScrollY (subtract 1) a
    | otherwise = a

moveDownAndPushEdge :: AppState -> AppState
moveDownAndPushEdge a@AppState{alocs=(Loc{sy=sy,cy=cy}:_)}
    | sy+cy >= bh = a
    | cy < ph-1 = updateCursorY (+1) a
    | otherwise = updateScrollY (+1) a
    where 
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
prevpage (a@AppState{abuf=b})
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

screen :: AppState -> Screen
screen a = scr where (Loc scr _ _) = loc a

-- | Enter a new screen, saving the old ui location on the stack.
enter :: Screen -> AppState -> AppState 
enter scr@BalanceScreen a  = updateData $ pushLoc Loc{scr=scr,sy=0,cy=0} a
enter scr@RegisterScreen a = updateData $ pushLoc Loc{scr=scr,sy=0,cy=0} a
enter scr@PrintScreen a    = updateData $ pushLoc Loc{scr=scr,sy=0,cy=0} a
enter scr@LedgerScreen a   = updateData $ pushLoc Loc{scr=scr,sy=0,cy=0} a

resetTrailAndEnter scr a = enter scr $ clearLocs a

-- | Regenerate the display data appropriate for the current screen.
updateData :: AppState -> AppState
updateData a@AppState{aopts=opts,aargs=args,aledger=l}
    | scr == BalanceScreen  = a{abuf=lines $ showBalanceReport opts [] l, aargs=[]}
    | scr == RegisterScreen = a{abuf=lines $ showRegisterReport opts args l}
    | scr == PrintScreen    = a{abuf=lines $ showEntries opts args l}
    | scr == LedgerScreen   = a{abuf=lines $ rawledgertext l}
    where scr = screen a

backout :: AppState -> AppState
backout a
    | screen a == BalanceScreen = a
    | otherwise = updateData $ popLoc a

drilldown :: AppState -> AppState
drilldown a
    | screen a == BalanceScreen  = enter RegisterScreen a{aargs=[currentAccountName a]}
    | screen a == RegisterScreen = scrollToEntry e $ enter PrintScreen a
    | screen a == PrintScreen   = enter LedgerScreen a
    | screen a == LedgerScreen   = a
    where e = currentEntry a

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
      dropsiblings (x:xs) = [x] ++ dropsiblings xs'
          where
            xs' = dropWhile moreindented xs
            moreindented = (>= myindent) . indentof
            myindent = indentof x
            indentof = length . takeWhile (==' ')

-- | If on the print screen, move the cursor to highlight the specified entry
-- (or a reasonable guess). Doesn't work.
scrollToEntry :: Entry -> AppState -> AppState
scrollToEntry e a@AppState{abuf=buf} = setCursorY cy $ setScrollY sy a
    where
      entryfirstline = head $ lines $ showEntry $ e
      halfph = pageHeight a `div` 2
      y = fromMaybe 0 $ findIndex (== entryfirstline) buf
      sy = max 0 $ y - halfph
      cy = y - sy

-- | Get the entry containing the transaction currently highlighted by the
-- cursor on the register screen (or best guess). Results undefined while
-- on other screens. Doesn't work.
currentEntry :: AppState -> Entry
currentEntry a@AppState{aledger=l,abuf=buf} = entryContainingTransaction a t
    where
      t = safehead nulltxn $ filter ismatch $ ledgerTransactions l
      ismatch t = date t == (parsedate $ take 10 datedesc)
                  && (take 70 $ showtxn False t nullmixedamt) == (datedesc ++ acctamt)
      datedesc = take 32 $ fromMaybe "" $ find (not . (" " `isPrefixOf`)) $ [safehead "" rest] ++ reverse above
      acctamt = drop 32 $ safehead "" rest
      safehead d ls = if null ls then d else head ls
      (above,rest) = splitAt y buf
      y = posY a

-- | Get the entry which contains the given transaction.
-- Will raise an error if there are problems.
entryContainingTransaction :: AppState -> Transaction -> Entry
entryContainingTransaction AppState{aledger=l} t = (entries $ rawledger l) !! entryno t

-- renderers

renderScreen :: AppState -> Picture
renderScreen (a@AppState{aw=w,ah=h,abuf=buf,amsg=msg}) =
    pic {pCursor = Cursor cx cy,
         pImage = mainimg
                  <->
                  renderStatus w msg
        }
    where 
      (cx, cy) = (0, cursorY a)
      sy = scrollY a
      -- trying for more speed
      mainimg = (vertcat $ map (render defaultattr) above)
               <->
               (render currentlineattr thisline)
               <->
               (vertcat $ map (render defaultattr) below)
      render attr = renderBS attr . B.pack
      (thisline,below) | null rest = (blankline,[])
                       | otherwise = (head rest, tail rest)
      (above,rest) = splitAt cy linestorender
      linestorender = map padclipline $ take (h-1) $ drop sy $ buf ++ replicate h blankline
      padclipline l = take w $ l ++ blankline
      blankline = replicate w ' '
--       mainimg = (renderString attr $ unlines $ above)
--           <->
--           (renderString reverseattr $ thisline)
--           <->
--           (renderString attr $ unlines $ below)
--       (above,(thisline:below)) 
--           | null ls   = ([],[""])
--           | otherwise = splitAt y ls
--       ls = lines $ fitto w (h-1) $ unlines $ drop as $ buf

padClipString :: Int -> Int -> String -> [String]
padClipString h w s = rows
    where
      rows = map padclipline $ take h $ lines s ++ replicate h blankline
      padclipline l = take w $ l ++ blankline
      blankline = replicate w ' '

renderString :: Attr -> String -> Image
renderString attr s = vertcat $ map (renderBS attr . B.pack) rows
    where
      rows = lines $ fitto w h s
      w = maximum $ map length $ ls
      h = length ls
      ls = lines s

renderStatus :: Int -> String -> Image
renderStatus w s = renderBS statusattr (B.pack $ take w (s ++ repeat ' ')) 


-- the all-important theming engine

theme = 1

(defaultattr, 
 currentlineattr, 
 statusattr
 ) = 
    case theme of
      1 -> ( -- restrained
           attr
          ,setBold attr
          ,setRV attr
          )
      2 -> ( -- colorful
           setRV attr
          ,setFG white $ setBG red $ attr
          ,setFG black $ setBG green $ attr
          )
      3 -> ( -- 
           setRV attr
          ,setFG white $ setBG red $ attr
          ,setRV attr
          )

halfbrightattr = setHalfBright attr
reverseattr = setRV attr
redattr = setFG red attr
greenattr = setFG green attr
reverseredattr = setRV $ setFG red attr
reversegreenattr= setRV $ setFG green attr

--     pic { pCursor = Cursor x y,
--           pImage = renderFill pieceA ' ' w y 
--           <->
--           renderHFill pieceA ' ' x <|> renderChar pieceA '@' <|> renderHFill pieceA ' ' (w - x - 1) 
--           <->
--           renderFill pieceA ' ' w (h - y - 1) 
--           <->
--           renderStatus w msg
--         }
