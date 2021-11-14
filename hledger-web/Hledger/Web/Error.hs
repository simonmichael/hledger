{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Define the web application's if something went wrong, in the usual Yesod style.

module Hledger.Web.Error where

import Yesod

import Hledger.Web.Settings (widgetFile)

newtype Error = Error { problem :: String }

-- This is where we define the one route of the application if
-- something went wrong. For a full explanation of the syntax,
-- please see: http://www.yesodweb.com/book/handler
mkYesod "Error" [parseRoutes|
/ ErrorR GET
|]

instance Yesod Error

-- | The error view.
getErrorR :: Handler Html
getErrorR = defaultLayout $ do
    Error problem <- getYesod
    setTitle "Error - hledger-web"
    $(widgetFile "error")
 
