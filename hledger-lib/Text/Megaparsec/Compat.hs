{-# LANGUAGE CPP, FlexibleContexts #-}

module Text.Megaparsec.Compat
(module Text.Megaparsec
#if MIN_VERSION_megaparsec(6,0,0)
,module Text.Megaparsec.Char
#endif
,MPErr
,mptext
,mpMkPos
,mpUnPos
,mpMkParseError
)
where

import qualified Data.Set as S
import Data.Text
import Text.Megaparsec

#if MIN_VERSION_megaparsec(6,0,0)

import Text.Megaparsec.Char
import Data.List.NonEmpty (fromList)
import Data.Void (Void)

-- | A basic parse error type.
type MPErr = ErrorFancy Void

-- | Parse and return some Text.  
mptext :: MonadParsec e Text m => Tokens Text -> m (Tokens Text) 
mptext = string

#else

import Text.Megaparsec.Prim (MonadParsec)

type MPErr = Dec

mptext :: MonadParsec e Text m => Text -> m Text
mptext = fmap pack . string . unpack

#endif

mpMkPos :: Int -> Pos
mpMkPos = 
#if MIN_VERSION_megaparsec(6,0,0)
            mkPos
#else
            unsafePos . fromIntegral 
#endif

mpUnPos :: Pos -> Int
mpUnPos = 
#if MIN_VERSION_megaparsec(6,0,0)
            unPos
#else
            fromIntegral . unPos 
#endif

mpMkParseError :: FilePath -> String -> ParseError Char String
mpMkParseError f s = 
#if MIN_VERSION_megaparsec(6,0,0)
  FancyError (fromList [initialPos f]) (S.singleton $ ErrorFail s)
#else
  (mempty :: ParseError Char String){errorCustom = S.singleton $ f ++ ": " ++ s}
#endif
