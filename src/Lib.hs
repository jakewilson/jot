{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Lib
    ( getNotes
    , JotDB(..)
    , Note(..)
    , saveNotes
    , Timestamp
    , NoteID
    , WordIndex
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import GHC.Generics

import System.Directory

data JotDB = JotDB
  { notes :: [Note]
  , words :: WordIndex
  }
  deriving (Generic, Show)

instance ToJSON JotDB where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON JotDB

type Timestamp = Integer
type NoteID    = Int
type WordIndex = M.Map T.Text [NoteID]

data Note = Note
  { timestamp :: Timestamp
  , note      :: T.Text
  , id        :: NoteID
  }
  deriving (Generic, Show)

instance ToJSON Note where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Note

jotPath :: IO FilePath
jotPath = (++ "/jot.json") <$> getHomeDirectory

emptyJot :: JotDB
emptyJot = JotDB [] M.empty

getNotes :: IO JotDB
getNotes = do
  path <- jotPath
  exists <- doesFileExist path
  if not exists
  then return emptyJot
  else do
    jsonDb <- readFile path
    case decode $ BLU.fromString jsonDb :: Maybe JotDB of
      (Just db) -> return db
      Nothing   -> do
        putStrLn "unrecognizable jot format - is your jotfile corrupted?"
        return emptyJot

saveNotes :: JotDB -> IO ()
saveNotes notes = jotPath >>= (`writeFile` (BLU.toString $ encode notes))
