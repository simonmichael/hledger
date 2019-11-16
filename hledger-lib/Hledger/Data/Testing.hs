{-# LANGUAGE OverloadedStrings #-}

module Hledger.Data.Testing
where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Test.QuickCheck

import Hledger.Data.Types

newtype ArbTag = ArbTag Tag deriving (Show)

instance Arbitrary ArbTag where
    arbitrary = do
        -- See https://hledger.org/journal.html#tags
        let arbNonSpace = (:[]) <$> (arbitraryPrintableChar `suchThat` (/= ',') `suchThat` (not . isSpace))
        tagName <- T.pack <$> listOf1 (arbitrary `suchThat` (\c -> isAlphaNum c || c `elem` ['-', '_']))
        tagValue <- T.pack <$> oneof [
                              arbNonSpace
                            , do
                                lead <- arbNonSpace
                                middle <- listOf (arbitraryPrintableChar `suchThat` (`notElem` ['\n', ',']))
                                trail <- arbNonSpace
                                return (lead ++ middle ++ trail)
                            ]
        return $ ArbTag (tagName, tagValue)


newtype ArbComment = ArbComment Text deriving (Show)

instance Arbitrary ArbComment where
    arbitrary = ArbComment <$> oneof [
                      T.pack <$> arbitrary
                    , do
                        cmnt <- T.pack <$> arbitrary
                        ArbTag (tagName, tagValue) <- arbitrary
                        return $ cmnt <> "\n" <> tagName <> ": " <> tagValue
                    ]
        -- TODO: support tagless comment
