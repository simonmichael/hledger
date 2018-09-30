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

  -- * Pretty-printing custom parse errors
  customErrorBundlePretty,


  -- * Final parse error types
  FinalParseError,
  FinalParseError',
  FinalParseErrorBundle,
  FinalParseErrorBundle',

  -- * Constructing final parse errors
  finalError,
  finalFancyFailure,
  finalFail,
  finalCustomFailure,

  -- * Handling errors from include files with final parse errors
  parseIncludeFile,
  attachSource,

  -- * Pretty-printing final parse errors
  finalErrorBundlePretty,
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

deriving instance (Eq (Token c), Ord (Token c), Ord c, Ord e) => Ord (ParseError c e)

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


--- * Final parse error types

-- | A parse error type intended for throwing parse errors without the
-- possiblity of backtracking. Intended for use as the error type in an
-- 'ExceptT' layer of the parser. The 'ExceptT' layer is responsible for
-- handling include files, so this type also records a stack of include
-- files in order to report the stack in parse errors.
--
-- In order to pretty-print our custom parse errors, we must bundle them
-- with their full source text and filepaths (the 'FinalBundleWithStack'
-- constructor). However, when an error is thrown from within a parser, we
-- do not have access to the full source, so we must hold the parse error
-- (the 'FinalError' constructor) until it can be joined with the source
-- text and its filepath by the parser's caller.

data FinalParseError' e
  = FinalError           (ParseError Text e)
  | FinalBundle          (ParseErrorBundle Text e)
  | FinalBundleWithStack (FinalParseErrorBundle' e)
  deriving (Show)

type FinalParseError = FinalParseError' CustomErr

-- A 'Monoid' instance is necessary for 'ExceptT (FinalParseError' e)' to
-- be an instance of Alternative and MonadPlus, which are required for the
-- use of e.g. the 'many' parser combinator. This monoid instance simply
-- takes the first (left-most) error.

instance Semigroup (FinalParseError' e) where
  e <> _ = e

instance Monoid (FinalParseError' e) where
  mempty = FinalError $ FancyError 0 $
            S.singleton (ErrorFail "default parse error")
  mappend = (<>)

-- | A type bundling a 'ParseError' with its full source file and a stack
-- of include file paths (for pretty printing).

data FinalParseErrorBundle' e = FinalParseErrorBundle'
  { finalErrorBundle :: ParseErrorBundle Text e
  , sourceFileStack  :: NE.NonEmpty FilePath
  } deriving (Show)

type FinalParseErrorBundle = FinalParseErrorBundle' CustomErr


--- * Constructing and throwing final parse errors

-- | Convert a "regular" parse error into a "final" parse error.

finalError :: ParseError Text e -> FinalParseError' e
finalError = FinalError

-- | Like 'fancyFailure', but as a "final" parse error.

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

-- | Like 'customFailure', but as a "final" parse error.

finalCustomFailure
  :: (MonadParsec e s m, MonadError (FinalParseError' e) m) => e -> m a
finalCustomFailure = finalFancyFailure . S.singleton . ErrorCustom


--- * Handling errors from include files with "final" parse errors

-- Some care must be taken for sources to be attached to the right parse
-- errors when dealing with include files, so we capture the necessary
-- procedure in this function.

parseIncludeFile
  :: forall st m a. Monad m
  => StateT st (ParsecT CustomErr Text (ExceptT FinalParseError m)) a
  -> st
  -> FilePath
  -> Text
  -> StateT st (ParsecT CustomErr Text (ExceptT FinalParseError m)) a
parseIncludeFile parser initState filepath text =
  catchError parser' handler
  where
    parser' = do
      eResult <- lift $ lift $
                  runParserT (evalStateT parser initState) filepath text
      case eResult of
        Left parseErrorBundle -> throwError $ FinalBundle parseErrorBundle
        Right result -> pure result

    handler e = throwError $ FinalBundleWithStack $ attachSource filepath text e


attachSource
  :: FilePath -> Text -> FinalParseError' e -> FinalParseErrorBundle' e
attachSource filePath sourceText finalParseError = case finalParseError of

    FinalError parseError ->
      let bundle = ParseErrorBundle
            { bundleErrors = parseError NE.:| []
            , bundlePosState = initialPosState filePath sourceText }
      in  FinalParseErrorBundle'
            { finalErrorBundle = bundle
            , sourceFileStack  = filePath NE.:| [] }

    FinalBundle peBundle -> FinalParseErrorBundle'
      { finalErrorBundle = peBundle
      , sourceFileStack  = filePath NE.:| [] }

    FinalBundleWithStack fpeBundle -> fpeBundle
      { sourceFileStack = filePath NE.<| sourceFileStack fpeBundle }


--- * Pretty-printing final parse errors

-- | Pretty-print a "final" parse error: print the stack of include files,
-- then apply the pretty-printer for custom parse errors.

finalErrorBundlePretty :: FinalParseErrorBundle' CustomErr -> String
finalErrorBundlePretty bundle =
     concatMap printIncludeFile (NE.init (sourceFileStack bundle))
  <> customErrorBundlePretty (finalErrorBundle bundle)
  where
    printIncludeFile path = "in file included from " <> path <> ",\n"


--- * Helpers

-- The "tab width" and "line prefix" are taken from the defaults defined
-- in 'initialState'.

initialPosState :: FilePath -> Text -> PosState Text
initialPosState filePath sourceText = PosState
  { pstateInput      = sourceText
  , pstateOffset     = 0
  , pstateSourcePos  = initialPos filePath
  , pstateTabWidth   = defaultTabWidth
  , pstateLinePrefix = "" }
