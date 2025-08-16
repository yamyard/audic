{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson
import Data.Pool (Pool)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Simple hiding (Proxy)
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude hiding (Word, id)

-- | Define the Word entity for the database
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Word
    word Text
    definition Text
    phonetic Text Maybe
    partOfSpeech Text Maybe
    example Text Maybe
    addedAt UTCTime
    deriving Show Generic
|]

-- | Response structure for a dictionary lookup
data DictionaryResponse = DictionaryResponse
  { word :: Text
  , phonetic :: Maybe Text
  , meanings :: [Meaning]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Meaning structure, containing part of speech and definitions
data Meaning = Meaning
  { partOfSpeech :: Text
  , definitions :: [Definition]
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Definition structure for word meanings
data Definition = Definition
  { definition :: Text
  , example :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

-- | Define the API routes
type API = Get '[HTML] Html
      :<|> "lookup" :> QueryParam "word" Text :> Get '[HTML] Html
      :<|> "add" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] Html
      :<|> "words" :> Get '[HTML] Html
      :<|> "static" :> Raw

indexPage :: Html
indexPage = docTypeHtml $ do
    H.head $ do
        H.title "Audic"
        H.meta ! A.charset "utf-8"
        H.style $ toHtml gnuStyles

    H.body $ do
        H.div ! A.class_ "container" $ do
            H.div ! A.class_ "header" $ do
                H.h1 "Audic"
                H.p ! A.class_ "subtitle" $
                    "A free software dictionary tool respecting your freedom"
                H.div ! A.class_ "gnu-notice" $ do
                    H.p "This is free software; you are free to change and redistribute it."
                    H.p "There is NO WARRANTY, to the extent permitted by law."

            H.div ! A.class_ "search-section" $ do
                H.h2 "Look up a word"
                H.form ! A.method "get" ! A.action "/lookup" $ do
                    H.input ! A.type_ "text" ! A.name "word" ! A.placeholder "Enter word to lookup"
                           ! A.class_ "word-input" ! A.required ""
                    H.button ! A.type_ "submit" ! A.class_ "btn" $ "Look up"

            H.div ! A.class_ "navigation" $ do
                H.a ! A.href "/words" ! A.class_ "nav-link" $ "View saved words"
                H.a ! A.href "https://www.gnu.org" ! A.class_ "nav-link" $ "GNU Project"

            H.div ! A.class_ "info-box" $ do
                H.h3 "About"
                H.p "This dictionary application allows you to:"
                H.ul $ do
                    H.li "Look up word definitions using external APIs"
                    H.li "Save new words to your local database"
                    H.li "Review your vocabulary collection"
                H.p ! A.class_ "license-note" $
                    "Licensed under GPL v3. Source code available for modification."

lookupPage :: Maybe DictionaryResponse -> Maybe Text -> Html
lookupPage Nothing (Just word) = docTypeHtml $ do
    H.head $ do
        H.title "GNU Dictionary - Word Not Found"
        H.style $ toHtml gnuStyles
    H.body $ do
        H.div ! A.class_ "container" $ do
            headerSection
            H.div ! A.class_ "error-section" $ do
                H.h2 $ "Word not found: " >> toHtml word
                H.p "The word could not be found in the external dictionary."
                H.a ! A.href "/" ! A.class_ "btn" $ "← Back to search"

lookupPage (Just resp) _ = docTypeHtml $ do
    H.head $ do
        H.title $ "GNU Dictionary - " >> toHtml (Main.word resp)
        H.style $ toHtml gnuStyles
    H.body $ do
        H.div ! A.class_ "container" $ do
            headerSection
            H.div ! A.class_ "word-result" $ do
                H.h2 ! A.class_ "word-title" $ toHtml (Main.word resp)
                case phonetic resp of
                    Just ph -> H.p ! A.class_ "phonetic" $ toHtml ph
                    Nothing -> mempty

                H.form ! A.method "post" ! A.action "/add" ! A.class_ "add-form" $ do
                    H.input ! A.type_ "hidden" ! A.name "word" ! A.value (toValue $ Main.word resp)
                    H.input ! A.type_ "hidden" ! A.name "phonetic" ! A.value
                        (toValue $ maybe "" T.unpack (phonetic resp))
                    H.button ! A.type_ "submit" ! A.class_ "btn add-btn" $
                        "Add to vocabulary"

                H.div ! A.class_ "meanings" $
                    mapM_ meaningToHtml (meanings resp)

            H.a ! A.href "/" ! A.class_ "btn" $ "← Back to search"

wordsPage :: [Entity Word] -> Html
wordsPage words = docTypeHtml $ do
    H.head $ do
        H.title "GNU Dictionary - Saved Words"
        H.style $ toHtml gnuStyles
    H.body $ do
        H.div ! A.class_ "container" $ do
            headerSection
            H.h2 "Your Vocabulary Collection"

            if null words
                then H.p ! A.class_ "empty-notice" $
                    "No words saved yet. Start building your vocabulary!"
                else H.div ! A.class_ "word-list" $
                    mapM_ wordEntryToHtml words

            H.a ! A.href "/" ! A.class_ "btn" $ "← Back to search"

-- | Helper function for creating the header section in all pages
headerSection :: Html
headerSection = H.div ! A.class_ "header" $ do
    H.h1 $ H.a ! A.href "/" ! A.style "text-decoration: none; color: inherit;" $
        "GNU Dictionary"
    H.p ! A.class_ "subtitle" $ "Free Software Dictionary Tool"

meaningToHtml :: Meaning -> Html
meaningToHtml meaning = H.div ! A.class_ "meaning" $ do
    H.h3 ! A.class_ "part-of-speech" $ toHtml (Main.partOfSpeech meaning)
    H.ol ! A.class_ "definitions" $
        mapM_ definitionToHtml (definitions meaning)

definitionToHtml :: Definition -> Html
definitionToHtml def = H.li ! A.class_ "definition-item" $ do
    H.p ! A.class_ "definition-text" $ toHtml (definition def)
    case example def of
        Just ex -> H.p ! A.class_ "example" $
            H.em $ "Example: " >> toHtml ex
        Nothing -> mempty

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

gnuStyles :: Text
gnuStyles = T.unlines
    [ "/* GNU Dictionary - Free Software Styling */"
    , "body {"
    , "    font-family: 'Liberation Serif', 'Times New Roman', serif;"
    , "    background-color: #ffffff;"
    , "    color: #000000;"
    , "    margin: 0;"
    , "    padding: 20px;"
    , "    line-height: 1.6;"
    , "}"
    , ""
    , ".container {"
    , "    max-width: 800px;"
    , "    margin: 0 auto;"
    , "    background-color: #ffffff;"
    , "    border: 2px solid #000000;"
    , "    padding: 20px;"
    , "}"
    , ""
    , ".header {"
    , "    text-align: center;"
    , "    border-bottom: 2px solid #000000;"
    , "    margin-bottom: 30px;"
    , "    padding-bottom: 20px;"
    , "}"
    , ""
    , "h1 {"
    , "    font-size: 2.5em;"
    , "    margin: 0;"
    , "    font-weight: bold;"
    , "    color: #000000;"
    , "}"
    , ""
    , ".subtitle {"
    , "    font-style: italic;"
    , "    margin: 10px 0;"
    , "    color: #444444;"
    , "}"
    , ""
    , ".gnu-notice {"
    , "    background-color: #f5f5f5;"
    , "    border: 1px solid #cccccc;"
    , "    padding: 10px;"
    , "    margin: 15px 0;"
    , "    font-size: 0.9em;"
    , "    text-align: left;"
    , "}"
    , ""
    , ".search-section {"
    , "    margin: 30px 0;"
    , "    text-align: center;"
    , "}"
    , ""
    , ".word-input {"
    , "    padding: 10px;"
    , "    font-size: 16px;"
    , "    border: 2px solid #000000;"
    , "    margin-right: 10px;"
    , "    width: 300px;"
    , "    font-family: 'Liberation Mono', monospace;"
    , "}"
    , ""
    , ".btn {"
    , "    padding: 10px 20px;"
    , "    background-color: #ffffff;"
    , "    border: 2px solid #000000;"
    , "    cursor: pointer;"
    , "    font-size: 16px;"
    , "    text-decoration: none;"
    , "    color: #000000;"
    , "    display: inline-block;"
    , "    margin: 5px;"
    , "    font-family: 'Liberation Sans', sans-serif;"
    , "}"
    , ""
    , ".btn:hover {"
    , "    background-color: #f0f0f0;"
    , "}"
    , ""
    , ".add-btn {"
    , "    background-color: #e6f3ff;"
    , "    border-color: #0066cc;"
    , "}"
    , ""
    , ".navigation {"
    , "    text-align: center;"
    , "    margin: 20px 0;"
    , "}"
    , ""
    , ".nav-link {"
    , "    margin: 0 15px;"
    , "    color: #0066cc;"
    , "    text-decoration: underline;"
    , "}"
    , ""
    , ".word-result {"
    , "    margin: 30px 0;"
    , "    border: 1px solid #000000;"
    , "    padding: 20px;"
    , "}"
    , ""
    , ".word-title {"
    , "    font-size: 2em;"
    , "    margin-bottom: 10px;"
    , "    color: #000000;"
    , "}"
    , ""
    , ".phonetic {"
    , "    font-family: 'Liberation Mono', monospace;"
    , "    color: #666666;"
    , "    font-style: italic;"
    , "}"
    , ""
    , ".meanings {"
    , "    margin-top: 20px;"
    , "}"
    , ""
    , ".meaning {"
    , "    margin: 20px 0;"
    , "    border-left: 3px solid #cccccc;"
    , "    padding-left: 15px;"
    , "}"
    , ""
    , ".part-of-speech {"
    , "    color: #0066cc;"
    , "    font-style: italic;"
    , "    margin-bottom: 10px;"
    , "}"
    , ""
    , ".definitions {"
    , "    margin-left: 0;"
    , "    padding-left: 20px;"
    , "}"
    , ""
    , ".definition-item {"
    , "    margin: 10px 0;"
    , "}"
    , ""
    , ".definition-text {"
    , "    margin: 5px 0;"
    , "}"
    , ""
    , ".example {"
    , "    color: #666666;"
    , "    margin: 5px 0 15px 0;"
    , "}"
    , ""
    , ".word-list {"
    , "    margin: 20px 0;"
    , "}"
    , ""
    , ".word-entry {"
    , "    border: 1px solid #cccccc;"
    , "    margin: 15px 0;"
    , "    padding: 15px;"
    , "    background-color: #fafafa;"
    , "}"
    , ""
    , ".saved-word {"
    , "    margin: 0 0 10px 0;"
    , "    color: #000000;"
    , "}"
    , ""
    , ".pos-tag {"
    , "    background-color: #e6f3ff;"
    , "    padding: 2px 8px;"
    , "    border: 1px solid #0066cc;"
    , "    font-size: 0.8em;"
    , "    margin-right: 10px;"
    , "}"
    , ""
    , ".date-added {"
    , "    font-size: 0.8em;"
    , "    color: #888888;"
    , "    margin-top: 10px;"
    , "}"
    , ""
    , ".info-box {"
    , "    border: 2px solid #000000;"
    , "    padding: 15px;"
    , "    margin: 20px 0;"
    , "    background-color: #f9f9f9;"
    , "}"
    , ""
    , ".license-note {"
    , "    font-size: 0.9em;"
    , "    font-style: italic;"
    , "    color: #666666;"
    , "}"
    , ""
    , ".error-section {"
    , "    text-align: center;"
    , "    margin: 40px 0;"
    , "    padding: 20px;"
    , "    border: 2px solid #cc0000;"
    , "    background-color: #fff5f5;"
    , "}"
    , ""
    , ".empty-notice {"
    , "    text-align: center;"
    , "    color: #666666;"
    , "    font-style: italic;"
    , "    margin: 40px 0;"
    , "}"
    , ""
    , ".add-form {"
    , "    margin: 15px 0;"
    , "    text-align: center;"
    , "}"
    ]

server :: Pool SqlBackend -> Server API
server pool = indexHandler
         :<|> lookupHandler pool
         :<|> addHandler pool
         :<|> wordsHandler pool
         :<|> serveDirectoryWebApp "static"

indexHandler :: Handler Html
indexHandler = return indexPage

lookupHandler :: Pool SqlBackend -> Maybe Text -> Handler Html
lookupHandler _ Nothing = return indexPage
lookupHandler pool (Just word) = do
    result <- liftIO $ lookupWord word
    return $ lookupPage result (Just word)

addHandler :: Pool SqlBackend -> [(Text, Text)] -> Handler Html
addHandler pool formData = do
    let wordText = lookup "word" formData
        phoneticText = lookup "phonetic" formData
    case wordText of
        Nothing -> return indexPage
        Just w -> do
            now <- liftIO getCurrentTime
            _ <- liftIO $ runSqlPool (insert $ Word w "Definition from lookup" phoneticText Nothing Nothing now) pool
            wordsHandler pool

wordsHandler :: Pool SqlBackend -> Handler Html
wordsHandler pool = do
    words <- liftIO $ runSqlPool (selectList [] [Desc WordAddedAt]) pool
    return $ wordsPage words

lookupWord :: Text -> IO (Maybe DictionaryResponse)
lookupWord word = do
    let url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ T.unpack word
    putStrLn $ "Looking up word: " ++ T.unpack word

    response <- httpJSON =<< parseRequest url
    let responseBody = getResponseBody response :: [DictionaryResponse]

    case responseBody of
        (dict:_) -> return $ Just dict
        [] -> return Nothing
  `catch` \(_ :: HttpException) -> do
    putStrLn $ "Failed to lookup word: " ++ T.unpack word
    return Nothing

app :: Pool SqlBackend -> Application
app pool = serve (Data.Proxy.Proxy :: Data.Proxy.Proxy API) (server pool)

main :: IO ()
main =
  runStdoutLoggingT $ withSqlitePool "dictionary.db" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
      liftIO $ do
        putStrLn "Database initialized successfully"
        putStrLn "Server running on http://localhost:8080"
        putStrLn "Press Ctrl+C to stop"
        putStrLn ""
        putStrLn "This program comes with ABSOLUTELY NO WARRANTY"
        run 8080 (app pool)
