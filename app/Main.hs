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

import Data.Char (isAlphaNum)
import Data.List (nub)

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

formatNotes :: [Note] -> T.Text
formatNotes ns = T.unlines $ map (format padLen) ns
  where
    -- the length to pad all note ids to
    -- so all notes are aligned like:
    -- 100 <note1>
    -- 99  <note2>
    -- 5   <note3>
    padLen = length $ show $ maximum $ map Lib.id ns

printRecent :: JotDB -> IO ()
printRecent = T.putStr . recentNotes

ts :: IO Timestamp
ts = round . (* 1000) <$> getPOSIXTime

nextNoteID :: [Note] -> NoteID
nextNoteID []     = 0
nextNoteID (n:ns) = Lib.id n + 1

jotWords :: T.Text -> [T.Text]
jotWords = map (T.filter isAlphaNum) . T.words

updateWordIdx :: WordIndex -> Note -> WordIndex
updateWordIdx idx (Note _ text nid) = foldr (addWord nid) idx $ jotWords text
  where
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

search :: JotDB -> [T.Text] -> [NoteID]
search (JotDB _ idx) xs =
  case foldr (\word -> (M.lookup word idx <>)) Nothing xs of
    Nothing    -> []
    (Just ids) -> nub ids

prompt :: IO ()
prompt = do
  T.putStr "> "
  hFlush stdout

notesFromIds :: JotDB -> [NoteID] -> [Note]
notesFromIds (JotDB notes _) ids = filter (\(Note _ _ id) -> id `elem` ids) notes

mainLoop :: JotDB -> IO ()
mainLoop jot = do
  prompt
  inp <- T.getLine
  case inp of
    ""     -> mainLoop jot
    "q"    -> saveNotes jot
    xs     -> case T.head xs of
      '/' -> do
        let terms = map T.toLower $ jotWords xs
        let ids = search jot terms
        let notes = notesFromIds jot ids
        T.putStrLn $ formatNotes notes
        mainLoop jot

      f -> do
        timestamp <- ts
        let jot' = addNote jot timestamp xs
        mainLoop jot'

main :: IO ()
main = do
  notes <- getNotes
  -- TODO add printRandom
  mainLoop notes
