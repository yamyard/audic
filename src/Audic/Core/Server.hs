{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Audic.Core.Server where

import Data.Pool (Pool)
import Data.Proxy (Proxy(..))
import Database.Persist.Sqlite
import Servant
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import Audic.Web

-- | Create the server with all handlers
server :: Pool SqlBackend -> Server API
server pool = indexHandler
  :<|> lookupHandler pool
  :<|> addHandler pool
  :<|> wordsHandler pool
  :<|> serveDirectoryWebApp "static"

-- | Create the WAI application
createApp :: Pool SqlBackend -> Application
createApp pool = serve (Proxy :: Proxy API) (server pool)
