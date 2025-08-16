{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Views.Words where

import Audic.Web.Views.Components
import Database.Persist
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Audic.Types

-- | Saved words listing page
wordsPage :: [Entity Audic.Types.Word] -> Html
wordsPage words = htmlDocument "Audic - Saved Words" $ pageContainer $ do
  headerSection
  H.h2 "Your Vocabulary Collection"

  if null words
      then H.p ! A.class_ "empty-notice" $
          "No words saved yet. Start building your vocabulary!"
      else H.div ! A.class_ "word-list" $
          mapM_ wordEntryToHtml words

  backButton
