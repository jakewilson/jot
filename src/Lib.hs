{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib
    ( getNotes
    , JotDB(..)
    , Note(..)
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import GHC.Generics

import System.Directory

data JotDB = JotDB
  { notes    :: [Note]
  , hashtags :: [T.Text]
  }
  deriving (Generic, Show)

instance ToJSON JotDB where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JotDB


data Note = Note
  { timestamp :: Integer
  , note     :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON Note where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Note

jotPath :: IO String
jotPath = (++ "/jot.json") <$> getHomeDirectory

getNotes :: IO JotDB
getNotes = do
  path <- jotPath
  -- if file doesn't exist, create it
  jsonDb <- readFile path
  case decode $ BLU.fromString jsonDb :: Maybe JotDB of
    (Just db) -> return db
    Nothing   -> return (JotDB [] []) -- TODO create file here
