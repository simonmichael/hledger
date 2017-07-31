-- | Paper over some differences between megaparsec 5 and 6,
-- making it possible to write code that supports both.

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
type MPErr = Void

-- | Make a simple parse error.
mpMkParseError :: FilePath -> String -> ParseError Char String
mpMkParseError f s = FancyError (fromList [initialPos f]) (S.singleton $ ErrorFail s)

-- | Make a Pos. With a negative argument, throws InvalidPosException (megaparsec >= 6)
-- or calls error (megaparsec < 6).
mpMkPos :: Int -> Pos
mpMkPos = mkPos 

-- | Unmake a Pos.
mpUnPos :: Pos -> Int
mpUnPos = unPos

-- | Parse and return some Text.  
mptext :: MonadParsec e Text m => Tokens Text -> m (Tokens Text) 
mptext = string

#else

import Text.Megaparsec.Prim (MonadParsec)

-- | A basic parse error type.
type MPErr = Dec

-- | Make a simple parse error.
mpMkParseError :: FilePath -> String -> ParseError Char String
mpMkParseError f s = (mempty :: ParseError Char String){errorCustom = S.singleton $ f ++ ": " ++ s}

-- | Make a Pos. With a negative argument, throws InvalidPosException (megaparsec >= 6)
-- or calls error (megaparsec < 6).
mpMkPos :: Int -> Pos
mpMkPos = unsafePos . fromIntegral 

-- | Unmake a Pos.
mpUnPos :: Pos -> Int
mpUnPos = fromIntegral . unPos 

-- | Parse and return some Text.  
mptext :: MonadParsec e Text m => Text -> m Text
mptext = fmap pack . string . unpack

#endif
