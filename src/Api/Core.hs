{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Core
  ( API
  , server
  , migrateAll
  ) where

import Prelude hiding (Word)
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
import Network.HTTP.Simple
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

-- 数据库实体（保持与你原文件一致）
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

-- API 类型（保持不变）
type API =
       Get '[HTML] Html
  :<|> "lookup" :> QueryParam "word" Text :> Get  '[HTML] Html
  :<|> "add"    :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] Html
  :<|> "words"  :> Get  '[HTML] Html
  :<|> "static" :> Raw

-- Server 组合（保持不变）
server :: Pool SqlBackend -> Server API
server pool =
       indexHandler
  :<|> lookupHandler pool
  :<|> addHandler pool
  :<|> wordsHandler pool
  :<|> serveDirectoryWebApp "static"

-- 处理函数（逻辑基本保持不变）
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
      -- 这里仍然使用占位定义，与你原来一致
      _ <- liftIO $ runSqlPool (insert $ Word w "Definition from lookup" phoneticText Nothing Nothing now) pool
      wordsHandler pool

wordsHandler :: Pool SqlBackend -> Handler Html
wordsHandler pool = do
  rows <- liftIO $ runSqlPool (selectList [] [Desc WordAddedAt]) pool
  let vms = map toVM rows
  pure $ wordsPage vms

-- 将 DB 记录映射到前端 ViewModel，避免 Frontend 依赖持久化类型
toVM :: Entity Word -> SavedWordVM
toVM (Entity _ w) = SavedWordVM
  { swWord        = wordWord w
  , swDefinition  = wordDefinition w
  , swPhonetic    = wordPhonetic w
  , swPartOfSpeech= wordPartOfSpeech w
  , swExample     = wordExample w
  , swAddedAt     = wordAddedAt w
  }

-- 外部字典查询（保持你的实现）
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
