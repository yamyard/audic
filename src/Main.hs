{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Servant (serve, Proxy(..))

import Api.Core (API, server, migrateAll)

main :: IO ()
main = do
  runStdoutLoggingT $ withSqlitePool "dictionary.db" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ do
      putStrLn "Application initialized successfully"
      putStrLn "Server running on http://localhost:8080"
      putStrLn "Press Ctrl+C to stop"
      putStrLn ""
      putStrLn "Copyright (C) 2025 Free Software Foundation"
      putStrLn "This program comes with ABSOLUTELY NO WARRANTY"
      run 8080 (serve (Proxy :: Proxy API) (server pool))
