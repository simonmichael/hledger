{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Megaparsec.Custom (
  -- * Custom parse error type
  CustomErr,

  -- * Constructing custom parse errors
  parseErrorAt,
  parseErrorAtRegion,

  -- * Pretty-printing custom parse errors
  customErrorBundlePretty,


  -- * "Final" parse errors
  FinalParseError,
  FinalParseError',
  FinalParseErrorBundle,
  FinalParseErrorBundle',

  -- * Constructing "final" parse errors
  finalError,
  finalFancyFailure,
  finalFail,
  finalCustomFailure,

  -- * Pretty-printing "final" parse errors
  finalErrorBundlePretty,
  attachSource,

  -- * Handling parse errors from include files with "final" parse errors
  parseIncludeFile,
)
where

import Prelude ()
import "base-compat-batteries" Prelude.Compat hiding (readFile)

import Control.Monad.Except
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Foldable (asum, toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text (Text)
import Text.Megaparsec


--- * Custom parse error type

-- | A custom error type for the parser. The type is specialized to
-- parsers of 'Text' streams.

data CustomErr
  -- | Fail with a message at a specific source position interval. The
  -- interval must be contained within a single line.
  = ErrorFailAt Int -- Starting offset
                Int -- Ending offset
                String -- Error message
  deriving (Show, Eq, Ord)

-- We require an 'Ord' instance for 'CustomError' so that they may be
-- stored in a 'Set'. The actual instance is inconsequential, so we just
-- derive it, but this requires an (orphan) instance for 'ParseError'.
-- Hopefully this does not cause any trouble.

instance ShowErrorComponent CustomErr where
  showErrorComponent (ErrorFailAt _ _ errMsg) = errMsg
  errorComponentLen (ErrorFailAt startOffset endOffset _) =
    endOffset - startOffset


--- * Constructing custom parse errors

-- | Fail at a specific source position, given by the raw offset from the
-- start of the input stream (the number of tokens processed at that
-- point).

parseErrorAt :: Int -> String -> CustomErr
parseErrorAt offset msg = ErrorFailAt offset (offset+1) msg

-- | Fail at a specific source interval, given by the raw offsets of its
-- endpoints from the start of the input stream (the numbers of tokens
-- processed at those points).
--
-- Note that care must be taken to ensure that the specified interval does
-- not span multiple lines of the input source, as this will not be
-- checked.

parseErrorAtRegion
  :: Int    -- ^ Start offset
  -> Int    -- ^ End end offset
  -> String -- ^ Error message
  -> CustomErr
parseErrorAtRegion startOffset endOffset msg =
  if startOffset < endOffset
    then ErrorFailAt startOffset endOffset msg
    else ErrorFailAt startOffset (startOffset+1) msg


--- * Pretty-printing custom parse errors

-- | Pretty-print our custom parse errors and display the line on which
-- the parse error occured.
--
-- Use this instead of 'errorBundlePretty' when custom parse errors are
-- thrown, otherwise the continuous highlighting in the pretty-printed
-- parse error will be displaced from its proper position.

customErrorBundlePretty :: ParseErrorBundle Text CustomErr -> String
customErrorBundlePretty errBundle =
  let errBundle' = errBundle
        { bundleErrors = fmap setCustomErrorOffset $ bundleErrors errBundle }
  in  errorBundlePretty errBundle'

  where
    setCustomErrorOffset
      :: ParseError Text CustomErr -> ParseError Text CustomErr
    setCustomErrorOffset err = case findCustomError err of
      Nothing -> err
      Just errFailAt@(ErrorFailAt startOffset _ _) ->
        FancyError startOffset $ S.singleton $ ErrorCustom errFailAt

    -- If any custom errors are present, arbitrarily take the first one
    -- (since only one custom error should be used at a time).
    findCustomError :: ParseError Text CustomErr -> Maybe CustomErr
    findCustomError err = case err of
      FancyError _ errSet -> 
        finds (\case {ErrorCustom e -> Just e; _ -> Nothing}) errSet
      _ -> Nothing

    finds :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
    finds f = asum . map f . toList


--- * "Final" parse errors
--
-- | A type representing "final" parse errors that cannot be backtracked
-- from and are guaranteed to halt parsing. The anti-backtracking
-- behaviour is implemented by an 'ExceptT' layer in the parser's monad
-- stack, using this type as the 'ExceptT' error type.
--
-- We have three goals for this type:
-- (1) it should be possible to convert any parse error into a "final"
-- parse error,
-- (2) it should be possible to take a parse error thrown from an include
-- file and re-throw it in the parent file, and
-- (3) the pretty-printing of "final" parse errors should be consistent
-- with that of ordinary parse errors, but should also report a stack of
-- files for errors thrown from include files.
--
-- In order to pretty-print a "final" parse error (goal 3), it must be
-- bundled with include filepaths and its full source text. When a "final"
-- parse error is thrown from within a parser, we do not have access to
-- the full source, so we must hold the parse error until it can be joined
-- with its source (and include filepaths, if it was thrown from an
-- include file) by the parser's caller.
--
-- A parse error with include filepaths and its full source text is
-- represented by the 'FinalParseErrorBundle' type, while a parse error in
-- need of either include filepaths, full source text, or both is
-- represented by the 'FinalParseError' type.

data FinalParseError' e
  -- a parse error thrown as a "final" parse error
  = FinalError           (ParseError Text e)
  -- a parse error obtained from running a parser, e.g. using 'runParserT'
  | FinalBundle          (ParseErrorBundle Text e)
  -- a parse error thrown from an include file
  | FinalBundleWithStack (FinalParseErrorBundle' e)
  deriving (Show)

type FinalParseError = FinalParseError' CustomErr

-- We need a 'Monoid' instance for 'FinalParseError' so that 'ExceptT
-- FinalParseError m' is an instance of Alternative and MonadPlus, which
-- is needed to use some parser combinators, e.g. 'many'.
--
-- This monoid instance simply takes the first (left-most) error.

instance Semigroup (FinalParseError' e) where
  e <> _ = e

instance Monoid (FinalParseError' e) where
  mempty = FinalError $ FancyError 0 $
            S.singleton (ErrorFail "default parse error")
  mappend = (<>)

-- | A type bundling a 'ParseError' with its full source text, filepath,
-- and stack of include files. Suitable for pretty-printing.
--
-- Megaparsec's 'ParseErrorBundle' type already bundles a parse error with
-- its full source text and filepath, so we just add a stack of include
-- files.

data FinalParseErrorBundle' e = FinalParseErrorBundle'
  { finalErrorBundle :: ParseErrorBundle Text e
  , includeFileStack :: [FilePath]
  } deriving (Show)

type FinalParseErrorBundle = FinalParseErrorBundle' CustomErr


--- * Constructing and throwing final parse errors

-- | Convert a "regular" parse error into a "final" parse error.

finalError :: ParseError Text e -> FinalParseError' e
finalError = FinalError

-- | Like megaparsec's 'fancyFailure', but as a "final" parse error.

finalFancyFailure
  :: (MonadParsec e s m, MonadError (FinalParseError' e) m)
  => S.Set (ErrorFancy e) -> m a
finalFancyFailure errSet = do
  offset <- getOffset
  throwError $ FinalError $ FancyError offset errSet

-- | Like 'fail', but as a "final" parse error.

finalFail
  :: (MonadParsec e s m, MonadError (FinalParseError' e) m) => String -> m a
finalFail = finalFancyFailure . S.singleton . ErrorFail

-- | Like megaparsec's 'customFailure', but as a "final" parse error.

finalCustomFailure
  :: (MonadParsec e s m, MonadError (FinalParseError' e) m) => e -> m a
finalCustomFailure = finalFancyFailure . S.singleton . ErrorCustom


--- * Pretty-printing "final" parse errors

-- | Pretty-print a "final" parse error: print the stack of include files,
-- then apply the pretty-printer for parse error bundles. Note that
-- 'attachSource' must be used on a "final" parse error before it can be
-- pretty-printed.

finalErrorBundlePretty :: FinalParseErrorBundle' CustomErr -> String
finalErrorBundlePretty bundle =
     concatMap showIncludeFilepath (includeFileStack bundle)
  <> customErrorBundlePretty (finalErrorBundle bundle)
  where
    showIncludeFilepath path = "in file included from " <> path <> ",\n"

-- | Supply a filepath and source text to a "final" parse error so that it
-- can be pretty-printed. You must ensure that you provide the appropriate
-- source text and filepath.

attachSource
  :: FilePath -> Text -> FinalParseError' e -> FinalParseErrorBundle' e
attachSource filePath sourceText finalParseError = case finalParseError of

  -- A parse error thrown directly with the 'FinalError' constructor
  -- requires both source and filepath.
  FinalError parseError ->
    let bundle = ParseErrorBundle
          { bundleErrors = parseError NE.:| []
          , bundlePosState = initialPosState filePath sourceText }
    in  FinalParseErrorBundle'
          { finalErrorBundle = bundle
          , includeFileStack  = [] }

  -- A 'ParseErrorBundle' already has the appropriate source and filepath
  -- and so needs neither.
  FinalBundle peBundle -> FinalParseErrorBundle'
    { finalErrorBundle = peBundle
    , includeFileStack = [] }

  -- A parse error from a 'FinalParseErrorBundle' was thrown from an
  -- include file, so we add the filepath to the stack.
  FinalBundleWithStack fpeBundle -> fpeBundle
    { includeFileStack = filePath : includeFileStack fpeBundle }


--- * Handling parse errors from include files with "final" parse errors

-- | Parse a file with the given parser and initial state, discarding the
-- final state and re-throwing any parse errors as "final" parse errors.

parseIncludeFile
  :: Monad m
  => StateT st (ParsecT CustomErr Text (ExceptT FinalParseError m)) a
  -> st
  -> FilePath
  -> Text
  -> StateT st (ParsecT CustomErr Text (ExceptT FinalParseError m)) a
parseIncludeFile parser initialState filepath text =
  catchError parser' handler
  where
    parser' = do
      eResult <- lift $ lift $
                  runParserT (evalStateT parser initialState) filepath text
      case eResult of
        Left parseErrorBundle -> throwError $ FinalBundle parseErrorBundle
        Right result -> pure result

    -- Attach source and filepath of the include file to its parse errors
    handler e = throwError $ FinalBundleWithStack $ attachSource filepath text e


--- * Helpers

-- Like megaparsec's 'initialState', but instead for 'PosState'. Used when
-- constructing 'ParseErrorBundle's. The values for "tab width" and "line
-- prefix" are taken from 'initialState'.

initialPosState :: FilePath -> Text -> PosState Text
initialPosState filePath sourceText = PosState
  { pstateInput      = sourceText
  , pstateOffset     = 0
  , pstateSourcePos  = initialPos filePath
  , pstateTabWidth   = defaultTabWidth
  , pstateLinePrefix = "" }
