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
import Text.Megaparsec (bundleErrors, eof, parseErrorTextPretty, runParser)
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
  let (postRes, displayRows) = validatePostings acctRes amtRes

  let descriptions = sort $ nub $ tdescription <$> jtxns j
      escapeJSSpecialChars = regexReplaceCI "</script>" "<\\/script>" -- #236
      listToJsonValueObjArrayStr = preEscapedString . escapeJSSpecialChars .
        encode . JSArray . fmap (\a -> JSObject $ toJSObject [("value", showJSON a)])
      journals = fst <$> jfiles j

  pure (validateTransaction dateRes descRes postRes, $(widgetFile "add-form"))
  where
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

validateTransaction ::
     FormResult Day
  -> FormResult Text
  -> FormResult [Posting]
  -> FormResult Transaction
validateTransaction dateRes descRes postingsRes =
  case makeTransaction <$> dateRes <*> descRes <*> postingsRes of
    FormSuccess txn -> case balanceTransaction Nothing txn of
      Left e -> FormFailure [T.pack e]
      Right txn' -> FormSuccess txn'
    x -> x
  where
    makeTransaction date desc postings =
      nulltransaction {tdate = date, tdescription = desc, tpostings = postings}


-- | Parse a list of postings out of a list of accounts and a corresponding list
-- of amounts
validatePostings ::
     FormResult [Text]
  -> FormResult [Text]
  -> (FormResult [Posting], [(Int, (Text, Text, Maybe Text, Maybe Text))])
validatePostings acctRes amtRes = let

  -- Zip accounts and amounts, fill in missing values and drop empty rows.
  rows :: [(Text, Text)]
  rows = filter (/= ("", "")) $ zipDefault "" (formSuccess [] acctRes) (formSuccess [] amtRes)

  -- Parse values and check for incomplete rows with only an account or an amount.
  -- The boolean in unfoldr state is for special handling of 'missingamt', where
  -- one row may have only an account and not an amount.
  postings :: [(Text, Text, Either (Maybe Text, Maybe Text) Posting)]
  postings = unfoldr go (True, rows)
  go (True, (x, ""):y:xs) = Just ((x, "", zipRow (checkAccount x) (Left "Missing amount")), (True, y:xs))
  go (True, (x, ""):xs) = Just ((x, "", zipRow (checkAccount x) (Right missingamt)), (False, xs))
  go (False, (x, ""):xs) = Just ((x, "", zipRow (checkAccount x) (Left "Missing amount")), (False, xs))
  go (_, ("", y):xs) = Just (("", y, zipRow (Left "Missing account") (checkAmount y)), (False, xs))
  go (_, (x, y):xs) = Just ((x, y, zipRow (checkAccount x) (checkAmount y)), (True, xs))
  go (_, []) = Nothing

  zipRow (Left e) (Left e') = Left (Just e, Just e')
  zipRow (Left e) (Right _) = Left (Just e, Nothing)
  zipRow (Right _) (Left e) = Left (Nothing, Just e)
  zipRow (Right acct) (Right amt) = Right (nullposting {paccount = acct, pamount = Mixed [amt]})

  errorToFormMsg = first (("Invalid value: " <>) . T.pack .
                          foldl (\s a -> s <> parseErrorTextPretty a) "" .
                          bundleErrors)
  checkAccount = errorToFormMsg . runParser (accountnamep <* eof) "" . T.strip
  checkAmount = errorToFormMsg . runParser (evalStateT (amountp <* eof) mempty) "" . T.strip

  -- Add errors to forms with zero or one rows if the form is not a FormMissing
  result :: [(Text, Text, Either (Maybe Text, Maybe Text) Posting)]
  result = case (acctRes, amtRes) of
    (FormMissing, FormMissing) -> postings
    _ -> case postings of
      [] -> [ ("", "", Left (Just "Missing account", Just "Missing amount"))
           , ("", "", Left (Just "Missing account", Nothing))
           ]
      [x] -> [x, ("", "", Left (Just "Missing account", Nothing))]
      xs -> xs

  -- Prepare rows for rendering - resolve Eithers into error messages and pad to
  -- at least four rows
  display' = flip fmap result $ \(acc, amt, res) -> case res of
    Left (mAccountErr, mAmountErr) -> (acc, amt, mAccountErr, mAmountErr)
    Right _ -> (acc, amt, Nothing, Nothing)
  display = display' ++ replicate (4 - length display') ("", "", Nothing, Nothing)

  -- And finally prepare the final FormResult [Posting]
  formResult = case traverse (\(_, _, x) -> x) result of
    Left _ -> FormFailure ["Postings validation failed"]
    Right xs -> FormSuccess xs

  in (formResult, zip [(1 :: Int)..] display)


zipDefault :: a -> [a] -> [a] -> [(a, a)]
zipDefault def (b:bs) (c:cs) = (b, c):(zipDefault def bs cs)
zipDefault def (b:bs) [] = (b, def):(zipDefault def bs [])
zipDefault def [] (c:cs) = (def, c):(zipDefault def [] cs)
zipDefault _ _ _ = []

formSuccess :: a -> FormResult a -> a
formSuccess def res = case res of
  FormSuccess x -> x
  _ -> def
