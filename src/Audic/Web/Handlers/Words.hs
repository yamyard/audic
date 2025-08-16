{-# LANGUAGE OverloadedStrings #-}

module Audic.Web.Handlers.Words where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Servant
import Text.Blaze.Html5 (Html)

import Audic.Types
import Audic.Web.Views.Index
import Audic.Web.Views.Words

-- | Handler for adding words to the database
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

-- | Handler for displaying saved words
wordsHandler :: Pool SqlBackend -> Handler Html
wordsHandler pool = do
  words <- liftIO $ runSqlPool (selectList [] [Desc WordAddedAt]) pool
  return $ wordsPage words
