{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Views.Index where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Audic.Web.Views.Components

indexPage :: Html
indexPage = htmlDocument "Audic" $ pageContainer $ do
  H.div ! A.class_ "header" $ do
      H.h1 "Audic"
      H.p ! A.class_ "subtitle" $
          "A free software dictionary tool respecting your freedom"
      H.div ! A.class_ "gnu-notice" $ do
          H.p "This is free software; you are free to change and redistribute it."
          H.p "There is NO WARRANTY, to the extent permitted by law."

  searchForm
  navigationSection

  H.div ! A.class_ "info-box" $ do
      H.h3 "About"
      H.p "This dictionary application allows you to:"
      H.ul $ do
          H.li "Look up word definitions using external APIs"
          H.li "Save new words to your local database"
          H.li "Review your vocabulary collection"
      H.p ! A.class_ "license-note" $
          "Licensed under GPL v3. Source code available for modification."
