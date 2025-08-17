{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.Core
  ( API
  , server
  , migrateAll
  ) where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Simple
import Prelude hiding (Word)
import Servant
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)

import Frontend.Core
  ( indexPage
  , lookupPage
  , wordsPage
  , DictionaryResponse(..)
  , Meaning(..)
  , Definition(..)
  , SavedWordVM(..)
  )

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

type API =
       Get '[HTML] Html
  :<|> "lookup" :> QueryParam "word" Text :> Get  '[HTML] Html
  :<|> "add"    :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] Html
  :<|> "words"  :> Get  '[HTML] Html
  :<|> "static" :> Raw

server :: Pool SqlBackend -> Server API
server pool =
       indexHandler
  :<|> lookupHandler pool
  :<|> addHandler pool
  :<|> wordsHandler pool
  :<|> serveDirectoryWebApp "static"

indexHandler :: Handler Html
indexHandler = pure indexPage

lookupHandler :: Pool SqlBackend -> Maybe Text -> Handler Html
lookupHandler _ Nothing     = pure indexPage
lookupHandler _ (Just word) = do
  result <- liftIO $ lookupWord word
  pure $ lookupPage result (Just word)

addHandler :: Pool SqlBackend -> [(Text, Text)] -> Handler Html
addHandler pool formData = do
  let wordText     = lookup "word"     formData
      phoneticText = lookup "phonetic" formData
  case wordText of
    Nothing -> pure indexPage
    Just w  -> do
      now <- liftIO getCurrentTime
      _ <- liftIO $ runSqlPool (insert $ Word w "Definition from lookup" phoneticText Nothing Nothing now) pool
      wordsHandler pool

wordsHandler :: Pool SqlBackend -> Handler Html
wordsHandler pool = do
  rows <- liftIO $ runSqlPool (selectList [] [Desc WordAddedAt]) pool
  let vms = map toVM rows
  pure $ wordsPage vms

toVM :: Entity Word -> SavedWordVM
toVM (Entity _ w) = SavedWordVM
  { swWord        = wordWord w
  , swDefinition  = wordDefinition w
  , swPhonetic    = wordPhonetic w
  , swPartOfSpeech= wordPartOfSpeech w
  , swExample     = wordExample w
  , swAddedAt     = wordAddedAt w
  }

lookupWord :: Text -> IO (Maybe DictionaryResponse)
lookupWord w = do
  let url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ T.unpack w
  putStrLn $ "Looking up word: " ++ T.unpack w
  (do
      response <- httpJSON =<< parseRequest url
      let responseBody = getResponseBody response :: [DictionaryResponse]
      case responseBody of
        (dict:_) -> pure $ Just dict
        []       -> pure Nothing
   ) `catch` \(_ :: HttpException) -> do
        putStrLn $ "Failed to lookup word: " ++ T.unpack w
        pure Nothing
