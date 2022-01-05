{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
  ( getNotes
  , JotDB(..)
  , Note(..)
  , saveNotes
  , Timestamp
  , saveNotes
  )

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO

format :: Int -> Note -> T.Text
format len n = T.unwords [(padN len $ Lib.id n), note n]

padN :: Int -> Int -> T.Text
padN len n = T.pack $ strN ++ (take desiredLen $ repeat ' ')
  where
    strN = show n
    desiredLen = len - (length strN)

recentNotes :: JotDB -> T.Text
recentNotes jot = T.unlines $ map (format len) n
  where
    n = take 5 $ notes jot
    len = length $ show $ maximum $ map Lib.id n

printRecent :: JotDB -> IO ()
printRecent = T.putStr . recentNotes

ts :: IO Timestamp
ts = round . (* 1000) <$> getPOSIXTime

addNote :: JotDB -> Timestamp -> T.Text -> JotDB
addNote (JotDB [] hs) t    note  = JotDB notes hs
  where notes = [Note t note 0]
addNote (JotDB (n:ns) hs) t note = JotDB notes hs
  where notes = Note t note (Lib.id n + 1) : n : ns

mainLoop :: JotDB -> IO ()
mainLoop jot = do
  printRecent jot -- TODO change to printRandom
  T.putStr "> "
  hFlush stdout
  inp <- T.words <$> T.getLine
  case inp of
    []         -> mainLoop jot
    ("q":_)    -> saveNotes jot
    xs         -> do
      timestamp <- ts
      let updatedNotes = addNote jot timestamp $ T.unwords xs
      mainLoop updatedNotes

main :: IO ()
main = do
  notes <- getNotes
  mainLoop notes
