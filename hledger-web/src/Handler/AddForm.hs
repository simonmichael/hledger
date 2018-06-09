-- | Add form data & handler. (The layout and js are defined in
-- Foundation so that the add form can be in the default layout for
-- all views.)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AddForm
  ( AddForm(..)
  , addForm
  , addFormHamlet
  ) where

import Data.List (sort, nub)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Blaze.Internal (preEscapedString)
import Text.Hamlet (hamletFile)
import Text.JSON
import Yesod (HtmlUrl, HandlerSite, RenderMessage)
import Yesod.Form

import Hledger

-- Part of the data required from the add form.
-- Don't know how to handle the variable posting fields with yesod-form yet.
-- XXX Variable postings fields
data AddForm = AddForm
    { addFormDate         :: Day
    , addFormDescription  :: Maybe Text
    , addFormJournalFile  :: Maybe Text
    } deriving Show

addForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => Day -> Journal -> FormInput m AddForm
addForm today j = AddForm
    <$> ireq (checkMMap (pure . validateDate) (T.pack . show) textField) "date"
    <*> iopt textField "description"
    <*> iopt (check validateJournalFile textField) "journal"
  where
    validateJournalFile :: Text -> Either FormMessage Text
    validateJournalFile f
      | T.unpack f `elem` journalFilePaths j = Right f
      | otherwise = Left $ MsgInvalidEntry $ "the selected journal file \"" <> f <> "\"is unknown"
    validateDate :: Text -> Either FormMessage Day
    validateDate s = case fixSmartDateStrEither' today (T.strip s) of
      Right d  -> Right d
      Left _   -> Left $ MsgInvalidEntry $ "could not parse date \"" <> s <> "\":"

addFormHamlet :: Journal -> t -> HtmlUrl t
addFormHamlet j r = $(hamletFile "templates/add-form.hamlet")
 where
  descriptions = sort $ nub $ tdescription <$> jtxns j
  accts = journalAccountNamesDeclaredOrImplied j
  escapeJSSpecialChars = regexReplaceCI "</script>" "<\\/script>" -- #236
  listToJsonValueObjArrayStr as  = preEscapedString $ escapeJSSpecialChars $ encode $ JSArray $ map (\a -> JSObject $ toJSObject [("value", showJSON a)]) as
  postingnums = [1..4 :: Int]
  filepaths = fst <$> jfiles j
