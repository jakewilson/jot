{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
  ( getNotes
  , JotDB(..)
  , Note(..)
  , saveNotes
  , NoteID
  , Timestamp
  , WordIndex
  , saveNotes
  )

import qualified Data.Char    as C
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.IO

format :: Int -> Note -> T.Text
format len n = T.unwords [padN len $ Lib.id n, note n]

padN :: Int -> Int -> T.Text
padN len n = T.pack $ strN ++ replicate desiredLen ' '
  where
    strN = show n
    desiredLen = len - length strN

recentNotes :: JotDB -> T.Text
recentNotes jot = T.unlines $ map (format len) n
  where
    n = take 5 $ notes jot
    len = length $ show $ maximum $ map Lib.id n

printRecent :: JotDB -> IO ()
printRecent = T.putStr . recentNotes

ts :: IO Timestamp
ts = round . (* 1000) <$> getPOSIXTime

nextNoteID :: [Note] -> NoteID
nextNoteID []     = 0
nextNoteID (n:ns) = Lib.id n + 1

updateWordIdx :: WordIndex -> Note -> WordIndex
updateWordIdx idx (Note _ text nid) = foldr (addWord nid) idx words
  where
    words = T.words $ T.filter (\c -> C.isAlphaNum c || C.isSpace c) text

    addWord :: NoteID -> T.Text -> WordIndex -> WordIndex
    addWord nid word = M.insertWith addIfNotThere (T.toLower word) [nid]

    addIfNotThere :: [NoteID] -> [NoteID] -> [NoteID]
    addIfNotThere xs [] = xs
    addIfNotThere [] _  = error "empty note id" -- should never happen
    addIfNotThere xs'@(x:xs) ys'@(y:ys)
      | x == y = ys'
      | otherwise = xs' ++ ys'

addNote :: JotDB -> Timestamp -> T.Text -> JotDB
addNote (JotDB ns idx) t noteTxt = JotDB (note : ns) (updateWordIdx idx note)
  where note = Note t noteTxt (nextNoteID ns)

mainLoop :: JotDB -> IO ()
mainLoop jot = do
  printRecent jot
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
  -- TODO add printRandom
  mainLoop notes
