{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Views.Components where

import Data.Text qualified as T
import Data.Time
import Database.Persist
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude hiding (Word, id)

import Audic.Types
import Audic.Web.Style

-- | Standard HTML document structure
htmlDocument :: H.Html -> H.Html -> H.Html
htmlDocument titleContent bodyContent = docTypeHtml $ do
  H.head $ do
      H.title titleContent
      H.meta ! A.charset "utf-8"
      H.style $ toHtml gnuStyles
  H.body bodyContent

-- | Container wrapper for page content
pageContainer :: H.Html -> H.Html
pageContainer content = H.div ! A.class_ "container" $ content

-- | Helper function for creating the header section in all pages
headerSection :: Html
headerSection = H.div ! A.class_ "header" $ do
  H.h1 $ H.a ! A.href "/" ! A.style "text-decoration: none; color: inherit;" $
      "Audic"
  H.p ! A.class_ "subtitle" $ "Free Software Dictionary Tool"

-- | Navigation links component
navigationSection :: Html
navigationSection = H.div ! A.class_ "navigation" $ do
  H.a ! A.href "/words" ! A.class_ "nav-link" $ "View saved words"
  H.a ! A.href "https://www.gnu.org" ! A.class_ "nav-link" $ "GNU Project"

-- | Search form component
searchForm :: Html
searchForm = H.div ! A.class_ "search-section" $ do
  H.h2 "Look up a word"
  H.form ! A.method "get" ! A.action "/lookup" $ do
      H.input ! A.type_ "text" ! A.name "word" ! A.placeholder "Enter word to lookup"
             ! A.class_ "word-input" ! A.required ""
      H.button ! A.type_ "submit" ! A.class_ "btn" $ "Look up"

-- | Back to search button
backButton :: Html
backButton = H.a ! A.href "/" ! A.class_ "btn" $ "← Back to search"

-- | Render a meaning to HTML
meaningToHtml :: Meaning -> Html
meaningToHtml meaning = H.div ! A.class_ "meaning" $ do
  H.h3 ! A.class_ "part-of-speech" $ toHtml (partOfSpeech meaning)
  H.ol ! A.class_ "definitions" $
      mapM_ definitionToHtml (definitions meaning)

-- | Render a definition to HTML
definitionToHtml :: Definition -> Html
definitionToHtml def = H.li ! A.class_ "definition-item" $ do
  H.p ! A.class_ "definition-text" $ toHtml (definition def)
  case example def of
      Just ex -> H.p ! A.class_ "example" $
          H.em $ "Example: " >> toHtml ex
      Nothing -> mempty

-- | Render a saved word entry to HTML
wordEntryToHtml :: Entity Word -> Html
wordEntryToHtml (Entity _ word) = H.div ! A.class_ "word-entry" $ do
  H.h3 ! A.class_ "saved-word" $ toHtml (wordWord word)
  case wordPhonetic word of
      Just ph -> H.p ! A.class_ "phonetic" $ toHtml ph
      Nothing -> mempty
  case wordPartOfSpeech word of
      Just pos -> H.span ! A.class_ "pos-tag" $ toHtml pos
      Nothing -> mempty
  H.p ! A.class_ "definition-text" $ toHtml (wordDefinition word)
  case wordExample word of
      Just ex -> H.p ! A.class_ "example" $ H.em $ toHtml ex
      Nothing -> mempty
  H.p ! A.class_ "date-added" $
      "Added: " >> toHtml (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (wordAddedAt word))
