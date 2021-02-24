{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-} -- new
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-} -- new

module Text.Megaparsec.Custom (
  -- * Custom parse error type
  CustomErr,

  -- * Failing with an arbitrary source position
  parseErrorAt,
  parseErrorAtRegion,

  -- * Re-parsing
  SourceExcerpt,
  getExcerptText,

  excerpt_,
  reparseExcerpt,

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
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Alt(..))
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
  -- | Re-throw parse errors obtained from the "re-parsing" of an excerpt
  -- of the source text.
  | ErrorReparsing
      (NE.NonEmpty (ParseError Text CustomErr)) -- Source fragment parse errors
  deriving (Show, Eq, Ord)

-- We require an 'Ord' instance for 'CustomError' so that they may be
-- stored in a 'Set'. The actual instance is inconsequential, so we just
-- derive it, but the derived instance requires an (orphan) instance for
-- 'ParseError'. Hopefully this does not cause any trouble.

deriving instance Ord (ParseError Text CustomErr)

-- Note: the pretty-printing of our 'CustomErr' type is only partally
-- defined in its 'ShowErrorComponent' instance; we perform additional
-- adjustments in 'customErrorBundlePretty'.

instance ShowErrorComponent CustomErr where
  showErrorComponent (ErrorFailAt _ _ errMsg) = errMsg
  showErrorComponent (ErrorReparsing _) = "" -- dummy value

  errorComponentLen (ErrorFailAt startOffset endOffset _) =
    endOffset - startOffset
  errorComponentLen (ErrorReparsing _) = 1 -- dummy value


--- * Failing with an arbitrary source position

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
-- not span multiple lines of the input source. This will not be checked.

parseErrorAtRegion
  :: Int    -- ^ Start offset
  -> Int    -- ^ End end offset
  -> String -- ^ Error message
  -> CustomErr
parseErrorAtRegion startOffset endOffset msg =
  if startOffset < endOffset
    then ErrorFailAt startOffset endOffset msg
    else ErrorFailAt startOffset (startOffset+1) msg


--- * Re-parsing

-- | A fragment of source suitable for "re-parsing". The purpose of this
-- data type is to preserve the content and source position of the excerpt
-- so that parse errors raised during "re-parsing" may properly reference
-- the original source.

data SourceExcerpt = SourceExcerpt Int  -- Offset of beginning of excerpt
                                   Text -- Fragment of source file

-- | Get the raw text of a source excerpt.

getExcerptText :: SourceExcerpt -> Text
getExcerptText (SourceExcerpt _ txt) = txt

-- | 'excerpt_ p' applies the given parser 'p' and extracts the portion of
-- the source consumed by 'p', along with the source position of this
-- portion. This is the only way to create a source excerpt suitable for
-- "re-parsing" by 'reparseExcerpt'.

-- This function could be extended to return the result of 'p', but we don't
-- currently need this.

excerpt_ :: MonadParsec CustomErr Text m => m a -> m SourceExcerpt
excerpt_ p = do
  offset <- getOffset
  (!txt, _) <- match p
  pure $ SourceExcerpt offset txt

-- | 'reparseExcerpt s p' "re-parses" the source excerpt 's' using the
-- parser 'p'. Parse errors raised by 'p' will be re-thrown at the source
-- position of the source excerpt.
--
-- In order for the correct source file to be displayed when re-throwing
-- parse errors, we must ensure that the source file during the use of
-- 'reparseExcerpt s p' is the same as that during the use of 'excerpt_'
-- that generated the source excerpt 's'. However, we can usually expect
-- this condition to be satisfied because, at the time of writing, the
-- only changes of source file in the codebase take place through include
-- files, and the parser for include files neither accepts nor returns
-- 'SourceExcerpt's.

reparseExcerpt
  :: Monad m
  => SourceExcerpt
  -> ParsecT CustomErr Text m a
  -> ParsecT CustomErr Text m a
reparseExcerpt (SourceExcerpt offset txt) p = do
  (_, res) <- lift $ runParserT' p (offsetInitialState offset txt)
  case res of
    Right result -> pure result
    Left errBundle -> customFailure $ ErrorReparsing $ bundleErrors errBundle

  where
    offsetInitialState :: Int -> s ->
#if MIN_VERSION_megaparsec(8,0,0)
      State s e
#else
      State s
#endif
    offsetInitialState initialOffset s = State
      { stateInput  = s
      , stateOffset = initialOffset
      , statePosState = PosState
        { pstateInput = s
        , pstateOffset = initialOffset
        , pstateSourcePos = initialPos ""
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = ""
        }
#if MIN_VERSION_megaparsec(8,0,0)
      , stateParseErrors = []
#endif
      }

--- * Pretty-printing custom parse errors

-- | Pretty-print our custom parse errors. It is necessary to use this
-- instead of 'errorBundlePretty' when custom parse errors are thrown.
--
-- This function intercepts our custom parse errors and applies final
-- adjustments ('finalizeCustomError') before passing them to
-- 'errorBundlePretty'. These adjustments are part of the implementation
-- of the behaviour of our custom parse errors.
--
-- Note: We must ensure that the offset of the 'PosState' of the provided
-- 'ParseErrorBundle' is no larger than the offset specified by a
-- 'ErrorFailAt' constructor. This is guaranteed if this offset is set to
-- 0 (that is, the beginning of the source file), which is the
-- case for 'ParseErrorBundle's returned from 'runParserT'.

customErrorBundlePretty :: ParseErrorBundle Text CustomErr -> String
customErrorBundlePretty errBundle =
  let errBundle' = errBundle { bundleErrors =
        NE.sortWith errorOffset $ -- megaparsec requires that the list of errors be sorted by their offsets
        bundleErrors errBundle >>= finalizeCustomError }
  in  errorBundlePretty errBundle'

  where
    finalizeCustomError
      :: ParseError Text CustomErr -> NE.NonEmpty (ParseError Text CustomErr)
    finalizeCustomError err = case findCustomError err of
      Nothing -> pure err

      Just errFailAt@(ErrorFailAt startOffset _ _) ->
        -- Adjust the offset
        pure $ FancyError startOffset $ S.singleton $ ErrorCustom errFailAt

      Just (ErrorReparsing errs) ->
        -- Extract and finalize the inner errors
        errs >>= finalizeCustomError

    -- If any custom errors are present, arbitrarily take the first one
    -- (since only one custom error should be used at a time).
    findCustomError :: ParseError Text CustomErr -> Maybe CustomErr
    findCustomError err = case err of
      FancyError _ errSet ->
        finds (\case {ErrorCustom e -> Just e; _ -> Nothing}) errSet
      _ -> Nothing

    finds :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe b
    finds f = getAlt . foldMap (Alt . f)


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
