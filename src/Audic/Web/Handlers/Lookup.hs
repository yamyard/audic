{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Handlers.Lookup where

import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Persist.Sqlite
import Network.HTTP.Simple
import Servant
import Text.Blaze.Html5 (Html)

import Audic.Types
import Audic.Web.Views.Index
import Audic.Web.Views.Lookup

-- | Handler for word lookup
lookupHandler :: Pool SqlBackend -> Maybe Text -> Handler Html
lookupHandler _ Nothing = return indexPage
lookupHandler pool (Just word) = do
  result <- liftIO $ lookupWord word
  return $ lookupPage result (Just word)

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
