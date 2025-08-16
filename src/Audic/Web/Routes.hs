{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Audic.Web.Routes where

import Data.Text (Text)
import Servant
import Servant.HTML.Blaze
import Text.Blaze.Html5 (Html)

-- | Main API routes
type API = IndexRoute
  :<|> LookupRoute
  :<|> AddRoute
  :<|> WordsRoute
  :<|> StaticRoute

type IndexRoute = Get '[HTML] Html
type LookupRoute = "lookup" :> QueryParam "word" Text :> Get '[HTML] Html
type AddRoute = "add" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[HTML] Html
type WordsRoute = "words" :> Get '[HTML] Html
type StaticRoute = "static" :> Raw
