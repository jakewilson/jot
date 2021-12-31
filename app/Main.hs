{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib (getNotes, JotDB(..), Note(..))

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.IO

recentNotes :: JotDB -> T.Text
recentNotes = T.unlines . map note . take 5 . notes

addNote :: JotDB -> T.Text -> JotDB
addNote (JotDB ns hs) note = JotDB ((Note 1000 note):ns) hs

mainLoop :: JotDB -> IO ()
mainLoop jot = do
  T.putStr "> "
  hFlush stdout
  inp <- T.words <$> T.getLine
  case inp of
    []         -> mainLoop jot
    ("q":_)    -> return ()
    ("qu":_)   -> return ()
    ("qui":_)  -> return ()
    ("quit":_) -> return ()
    xs         -> do
      let updatedNotes = addNote jot $ T.unwords xs
      T.putStr $ recentNotes updatedNotes
      mainLoop updatedNotes

main = do
  notes <- getNotes
  mainLoop notes
  -- TODO save notes
