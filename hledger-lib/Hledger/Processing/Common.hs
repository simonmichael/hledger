{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Hledger.Processing.Common (
    addMissingTags
)
where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Hledger.Data.Types
import Hledger.Data.Posting
import Hledger.Read.Common

-- $setup
-- >>> import Hledger.Data.Testing
-- >>> :set -XOverloadedStrings
-- >>> includes xs = all (`elem` xs)

-- | Ensure that comment contains all tags
--
-- New tag included in updated comment:
-- prop> \(ArbComment cmnt) (ArbTag tag') -> tag' `elem` scanTags (addMissingTags [tag'] cmnt)
--
-- All tags from original comment preserved:
-- prop> \(ArbComment cmnt) (ArbTag tag') -> scanTags (addMissingTags [tag'] cmnt) `includes` scanTags cmnt
--
-- Adding existing tags have no effect:
-- prop> \(ArbComment cmnt) -> scanTags (addMissingTags (scanTags cmnt) cmnt) == cmnt
-- prop> \(ArbTag tag) -> let cmnt = addMissingTags [tag] "" in addMissingTags [tag] cmnt == cmnt
addMissingTags :: [Tag] -> Text -> Text
addMissingTags (nub -> tags) cmnt = foldr (flip commentAddTag) cmnt tags'
    where
        tags' = filter (not. (`elem` tags0)) tags
        tags0 = scanTags cmnt

-- | Get tags from comment
--
-- >>> scanTags "abc def\nghi"
-- []
-- >>> scanTags "abc:"
-- [("abc","")]
-- >>> scanTags "abc def:"
-- [("def","")]
-- >>> scanTags "def:, abc: g hi, z"
-- [("def",""),("abc","g hi")]
scanTags :: Text -> [Tag]
scanTags = concatMap scanLine . T.lines  where
    scanLine = either (const []) id . runTextParser commenttagsp
