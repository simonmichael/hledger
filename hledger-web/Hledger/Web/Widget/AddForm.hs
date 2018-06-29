{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Widget.AddForm
  ( addForm
  , addModal
  ) where

import Control.Monad.State.Strict (evalStateT)
import Data.Bifunctor (first)
import Data.List (dropWhileEnd, nub, sort, unfoldr)
import Data.Maybe (isJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Text.Blaze.Internal (Markup, preEscapedString)
import Text.JSON
import Text.Megaparsec (eof, parseErrorPretty, runParser)
import Yesod

import Hledger
import Hledger.Web.Settings (widgetFile)

addModal ::
     ( MonadWidget m
     , r ~ Route (HandlerSite m)
#if MIN_VERSION_yesod(1,6,0)
     , m ~ WidgetFor (HandlerSite m)
#else
     , m ~ WidgetT (HandlerSite m) IO
#endif
     , RenderMessage (HandlerSite m) FormMessage
     )
  => r -> Journal -> Day -> m ()
addModal addR j today = do
  (addView, addEnctype) <- generateFormPost (addForm j today)
  [whamlet|
<div .modal #addmodal tabindex="-1" role="dialog" aria-labelledby="addLabel" aria-hidden="true">
  <div .modal-dialog .modal-lg>
    <div .modal-content>
      <div .modal-header>
        <button type="button" .close data-dismiss="modal" aria-hidden="true">&times;
        <h3 .modal-title #addLabel>Add a transaction
      <div .modal-body>
        <form#addform.form action=@{addR} method=POST enctype=#{addEnctype}>
          ^{addView}
|]

addForm ::
     (site ~ HandlerSite m, RenderMessage site FormMessage, MonadHandler m)
  => Journal
  -> Day
  -> Markup
#if MIN_VERSION_yesod(1,6,0)
  -> MForm m (FormResult Transaction, WidgetFor site ())
#else
  -> MForm m (FormResult Transaction, WidgetT site IO ())
#endif
addForm j today = identifyForm "add" $ \extra -> do
  (dateRes, dateView) <- mreq dateField dateFS Nothing
  (descRes, descView) <- mreq textField descFS Nothing
  (acctRes, _) <- mreq listField acctFS Nothing
  (amtRes, _) <- mreq listField amtFS Nothing

  let (msgs', postRes) = case validatePostings <$> acctRes <*> amtRes of
        FormSuccess (Left es) -> (es, FormFailure ["Postings validation failed"])
        FormSuccess (Right xs) -> ([], FormSuccess xs)
        FormMissing -> ([], FormMissing)
        FormFailure es -> ([], FormFailure es)
      msgs = zip [(1 :: Int)..] $ msgs' ++ replicate (4 - length msgs') ("", "", Nothing, Nothing)

  let descriptions = sort $ nub $ tdescription <$> jtxns j
      escapeJSSpecialChars = regexReplaceCI "</script>" "<\\/script>" -- #236
      listToJsonValueObjArrayStr = preEscapedString . escapeJSSpecialChars .
        encode . JSArray . fmap (\a -> JSObject $ toJSObject [("value", showJSON a)])
      journals = fst <$> jfiles j

  pure (makeTransaction <$> dateRes <*> descRes <*> postRes, $(widgetFile "add-form"))
  where
    makeTransaction date desc postings =
      nulltransaction {tdate = date, tdescription = desc, tpostings = postings}

    dateFS = FieldSettings "date" Nothing Nothing (Just "date")
      [("class", "form-control input-lg"), ("placeholder", "Date")]
    descFS = FieldSettings "desc" Nothing Nothing (Just "description")
      [("class", "form-control input-lg typeahead"), ("placeholder", "Description"), ("size", "40")]
    acctFS = FieldSettings "amount" Nothing Nothing (Just "account") []
    amtFS = FieldSettings "amount" Nothing Nothing (Just "amount") []
    dateField = checkMMap (pure . validateDate) (T.pack . show) textField
    validateDate s =
      first (const ("Invalid date format" :: Text)) $
      fixSmartDateStrEither' today (T.strip s)

    listField = Field
      { fieldParse = const . pure . Right . Just . dropWhileEnd T.null
      , fieldView = error "Don't render using this!"
      , fieldEnctype = UrlEncoded
      }

validatePostings :: [Text] -> [Text] -> Either [(Text, Text, Maybe Text, Maybe Text)] [Posting]
validatePostings a b =
  case traverse id $ (\(_, _, x) -> x) <$> postings of
    Left _ -> Left $ foldr catPostings [] postings
    Right [] -> Left
      [ ("", "", Just "Missing account", Just "Missing amount")
      , ("", "", Just "Missing account", Nothing)
      ]
    Right [p] -> Left
      [ (paccount p, T.pack . showMixedAmountWithoutPrice $ pamount p, Nothing, Nothing)
      , ("", "", Just "Missing account", Nothing)
      ]
    Right xs -> Right xs
  where
    postings = unfoldr go (True, a, b)

    go (_, x:xs, y:ys) = Just ((x, y, zipPosting (validateAccount x) (validateAmount y)), (True, xs, ys))
    go (True, x:y:xs, []) = Just ((x, "", zipPosting (validateAccount x) (Left "Missing amount")), (True, y:xs, []))
    go (True, x:xs, []) = Just ((x, "", zipPosting (validateAccount x) (Right missingamt)), (False, xs, []))
    go (False, x:xs, []) = Just ((x, "", zipPosting (validateAccount x) (Left "Missing amount")), (False, xs, []))
    go (_, [], y:ys) = Just (("", y, zipPosting (Left "Missing account") (validateAmount y)), (False, [], ys))
    go (_, [], []) = Nothing

    zipPosting = zipEither (\acc amt -> nullposting {paccount = acc, pamount = Mixed [amt]})

    catPostings (t, t', Left (e, e')) xs = (t, t', e, e') : xs
    catPostings (t, t', Right _) xs = (t, t', Nothing, Nothing) : xs

    errorToFormMsg = first (("Invalid value: " <>) . T.pack . parseErrorPretty)
    validateAccount = errorToFormMsg . runParser (accountnamep <* eof) "" . T.strip
    validateAmount = errorToFormMsg . runParser (evalStateT (amountp <* eof) mempty) "" . T.strip

-- Modification of Align, from the `these` package
zipEither :: (a -> a' -> r) -> Either e a -> Either e' a' -> Either (Maybe e, Maybe e') r
zipEither f a b = case (a, b) of
  (Right a', Right b') -> Right (f a' b')
  (Left a', Right _) -> Left (Just a', Nothing)
  (Right _, Left b') -> Left (Nothing, Just b')
  (Left a', Left b') -> Left (Just a', Just b')
