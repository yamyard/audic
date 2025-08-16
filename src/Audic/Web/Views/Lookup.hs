{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Views.Lookup where

import Data.Text (Text)
import Data.Text qualified as T
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Audic.Types
import Audic.Web.Views.Components

-- | Word lookup result page
lookupPage :: Maybe DictionaryResponse -> Maybe Text -> Html
lookupPage Nothing (Just word) =
  htmlDocument "Audic - Word Not Found" $ pageContainer $ do
    headerSection
    H.div ! A.class_ "error-section" $ do
        H.h2 $ "Word not found: " >> toHtml word
        H.p "The word could not be found in the external dictionary."
        backButton

lookupPage (Just resp) _ =
  htmlDocument ("Audic - " >> toHtml (Audic.Types.word resp)) $
    pageContainer $ do
      headerSection
      H.div ! A.class_ "word-result" $ do
          H.h2 ! A.class_ "word-title" $ toHtml (Audic.Types.word resp)
          case phonetic resp of
              Just ph -> H.p ! A.class_ "phonetic" $ toHtml ph
              Nothing -> mempty

          H.form ! A.method "post" ! A.action "/add" ! A.class_ "add-form" $ do
              H.input ! A.type_ "hidden" ! A.name "word" ! A.value (toValue $ Audic.Types.word resp)
              H.input ! A.type_ "hidden" ! A.name "phonetic" ! A.value
                  (toValue $ maybe "" T.unpack (phonetic resp))
              H.button ! A.type_ "submit" ! A.class_ "btn add-btn" $
                  "Add to vocabulary"

          H.div ! A.class_ "meanings" $
              mapM_ meaningToHtml (meanings resp)

      backButton

lookupPage Nothing Nothing =
  htmlDocument "Audic" $ pageContainer $ do
    headerSection
    searchForm
