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

recentNotes :: JotDB -> T.Text
recentNotes = T.unlines . map note . take 5 . notes

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
