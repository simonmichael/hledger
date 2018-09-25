{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Megaparsec.Custom (
  -- * Custom parse error type
  CustomErr,

  -- * Constructing custom parse errors
  parseErrorAt,
  parseErrorAtRegion,
  withSource,

  -- * Pretty-printing custom parse errors
  customParseErrorPretty
)
where

import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (readFile)

import Data.Foldable (asum, toList)
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as S
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec


--- * Custom parse error type

-- | A custom error type for the parser. The type is specialized to
-- parsers of 'Text' streams.

data CustomErr
  -- | Fail with a message at a specific source position interval. The
  -- interval must be contained within a single line.
  = ErrorFailAt SourcePos -- Starting position
                Pos -- Ending position (column; same line as start)
                String -- Error message
  -- | Attach a source file to a parse error (for error reporting from
  -- include files, e.g. with the 'region' parser combinator)
  | ErrorWithSource Text -- Source file contents
                    (ParseError Char CustomErr) -- The original
  deriving (Show, Eq, Ord)

-- We require an 'Ord' instance for 'CustomError' so that they may be
-- stored in a 'Set'. The actual instance is inconsequential, so we just
-- derive it, but this requires an (orphan) instance for 'ParseError'.
-- Hopefully this does not cause any trouble.

deriving instance (Ord c, Ord e) => Ord (ParseError c e)

instance ShowErrorComponent CustomErr where
  showErrorComponent (ErrorFailAt _ _ errMsg) = errMsg
  showErrorComponent (ErrorWithSource _ e) = parseErrorTextPretty e


--- * Constructing custom parse errors

-- | Fail at a specific source position.

parseErrorAt :: SourcePos -> String -> CustomErr
parseErrorAt pos msg = ErrorFailAt pos (sourceColumn pos) msg

-- | Fail at a specific source interval (within a single line). The
-- interval is inclusive on the left and exclusive on the right; that is,
-- it spans from the start position to just before (and not including) the
-- end position.

parseErrorAtRegion
  :: SourcePos -- ^ Start position
  -> SourcePos -- ^ End position
  -> String    -- ^ Error message
  -> CustomErr
parseErrorAtRegion startPos endPos msg =
  let startCol = sourceColumn startPos
      endCol' = mkPos $ subtract 1 $ unPos $ sourceColumn endPos
      endCol = if startCol <= endCol'
                    && sourceLine startPos == sourceLine endPos
               then endCol' else startCol
  in  ErrorFailAt startPos endCol msg

-- | Attach a source file to a parse error. Intended for use with the
-- 'region' parser combinator.

withSource :: Text -> ParseError Char CustomErr -> ParseError Char CustomErr
withSource s e =
  FancyError (errorPos e) $ S.singleton $ ErrorCustom $ ErrorWithSource s e


--- * Pretty-printing custom parse errors

-- | Pretty-print our custom parse errors and display the line on which
-- the parse error occured. Use this instead of 'parseErrorPretty'.
--
-- If any custom errors are present, arbitrarily take the first one (since
-- only one custom error should be used at a time).

customParseErrorPretty :: Text -> ParseError Char CustomErr -> String
customParseErrorPretty source err = case findCustomError err of
  Nothing -> customParseErrorPretty' source err pos1

  Just (ErrorWithSource customSource customErr) ->
    customParseErrorPretty customSource customErr

  Just (ErrorFailAt sourcePos col errMsg) ->
    let newPositionStack = sourcePos NE.:| NE.tail (errorPos err)
        errorIntervalLength = mkPos $ max 1 $
          unPos col - unPos (sourceColumn sourcePos) + 1

        newErr :: ParseError Char Void
        newErr = FancyError newPositionStack (S.singleton (ErrorFail errMsg))

    in  customParseErrorPretty' source newErr errorIntervalLength

  where
    findCustomError :: ParseError Char CustomErr -> Maybe CustomErr
    findCustomError err = case err of
      FancyError _ errSet -> 
        finds (\case {ErrorCustom e -> Just e; _ -> Nothing}) errSet
      _ -> Nothing

    finds :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
    finds f = asum . map f . toList


--- * Modified Megaparsec source

-- The below code has been copied from Megaparsec (v.6.4.1,
-- Text.Megaparsec.Error) and modified to suit our needs. These changes are
-- indicated by square brackets. The following copyright notice, conditions,
-- and disclaimer apply to all code below this point.
--
-- Copyright © 2015–2018 Megaparsec contributors<br>
-- Copyright © 2007 Paolo Martini<br>
-- Copyright © 1999–2000 Daan Leijen
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright notice,
--   this list of conditions and the following disclaimer in the documentation
--   and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
-- OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
-- NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
-- INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
-- OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- | Pretty-print a 'ParseError Char CustomErr' and display the line on
-- which the parse error occurred. The rendered 'String' always ends with
-- a newline.

customParseErrorPretty'
  :: ( ShowToken (Token s)
     , LineToken (Token s)
     , ShowErrorComponent e
     , Stream s )
  => s                 -- ^ Original input stream
  -> ParseError (Token s) e -- ^ Parse error to render
  -> Pos               -- ^ Length of error interval [added]
  -> String            -- ^ Result of rendering
customParseErrorPretty' = customParseErrorPretty_ defaultTabWidth


customParseErrorPretty_
  :: forall s e.
     ( ShowToken (Token s)
     , LineToken (Token s)
     , ShowErrorComponent e
     , Stream s )
  => Pos               -- ^ Tab width
  -> s                 -- ^ Original input stream
  -> ParseError (Token s) e -- ^ Parse error to render
  -> Pos               -- ^ Length of error interval [added]
  -> String            -- ^ Result of rendering
customParseErrorPretty_ w s e l =
  sourcePosStackPretty (errorPos e) <> ":\n" <>
    padding <> "|\n" <>
    lineNumber <> " | " <> rline <> "\n" <>
    padding <> "| " <> rpadding <> highlight <> "\n" <> -- [added `highlight`]
    parseErrorTextPretty e
  where
    epos       = NE.head (errorPos e) -- [changed from NE.last to NE.head]
    lineNumber = (show . unPos . sourceLine) epos
    padding    = replicate (length lineNumber + 1) ' '
    rpadding   = replicate (unPos (sourceColumn epos) - 1) ' '
    highlight  = replicate (unPos l) '^' -- [added]
    rline      =
      case rline' of
        [] -> "<empty line>"
        xs -> expandTab w xs
    rline'     = fmap tokenAsChar . chunkToTokens (Proxy :: Proxy s) $
      selectLine (sourceLine epos) s

-- | Select a line from input stream given its number.

selectLine
  :: forall s. (LineToken (Token s), Stream s)
  => Pos               -- ^ Number of line to select
  -> s                 -- ^ Input stream
  -> Tokens s          -- ^ Selected line
selectLine l = go pos1
  where
    go !n !s =
      if n == l
        then fst (takeWhile_ notNewline s)
        else go (n <> pos1) (stripNewline $ snd (takeWhile_ notNewline s))
    notNewline = not . tokenIsNewline
    stripNewline s =
      case take1_ s of
        Nothing -> s
        Just (_, s') -> s'

-- | Replace tab characters with given number of spaces.

expandTab
  :: Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go !n xs       = ' ' : go (n - 1) xs
    w              = unPos w'

