module Audic.Web.Handlers.Index where

import Servant
import Text.Blaze.Html5 (Html)

import Audic.Web.Views.Index

-- | Handler for the index page
indexHandler :: Handler Html
indexHandler = return indexPage
