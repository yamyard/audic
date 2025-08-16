{-# LANGUAGE OverloadedStrings #-}

module Audic.App where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool (Pool)
import Data.Proxy (Proxy(..))
import Data.Text qualified as T
import Database.Persist.Sqlite
import Network.HTTP.Simple hiding (Proxy)
import Network.Wai.Handler.Warp
import Servant

import Audic.Types
import Audic.Core

app :: IO ()
app = do
  runStdoutLoggingT $ withSqlitePool "dictionary.db" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      putStrLn "Database initialized successfully"
      putStrLn "Server running on http://localhost:8080"
      putStrLn "Press Ctrl+C to stop"
      putStrLn ""
      putStrLn "Copyright (C) 2025 Free Software Foundation"
      putStrLn "This program comes with ABSOLUTELY NO WARRANTY"
      run 8080 (createApp pool)
