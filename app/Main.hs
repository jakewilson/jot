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

import System.IO

recentNotes :: JotDB -> T.Text
recentNotes = T.unlines . map note . take 5 . notes

ts :: Timestamp
ts = 1000

addNote :: JotDB -> T.Text -> JotDB
addNote (JotDB [] hs)     note = JotDB notes hs
  where notes = [Note ts note 0]
addNote (JotDB (n:ns) hs) note = JotDB notes hs
  where notes = Note ts note (Lib.id n + 1) : n : ns

mainLoop :: JotDB -> IO ()
mainLoop jot = do
  T.putStr "> "
  hFlush stdout
  inp <- T.words <$> T.getLine
  case inp of
    []         -> mainLoop jot
    ("q":_)    -> saveNotes jot
    xs         -> do
      let updatedNotes = addNote jot $ T.unwords xs
      T.putStr $ recentNotes updatedNotes
      mainLoop updatedNotes

main :: IO ()
main = do
  notes <- getNotes
  mainLoop notes
