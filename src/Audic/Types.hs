{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Audic.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics
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
