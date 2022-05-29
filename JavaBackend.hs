module JavaBackend where

import Data.List
import Data.List.Utils
import System.IO

import JSynEffects

import HMusic

type EInstrument = (Maybe [Effect], Instrument)

-- TODO: Make sure the sample isn't loaded multiple times into memory in case
-- it is duplicated (e.g. there's a guitar.wav with echo and one without).

compileJava :: Track -> Float -> String -> IO ()
compileJava track bpm name = do
  template <- songTemplate
  let song = genJava track bpm name template
  writeFile (name ++ ".java") song

-- Java class template for a compiled song.
songTemplate :: IO String
songTemplate = do
  handle <- openFile "Template.java" ReadMode
  contents <- hGetContents handle
  return contents

-- Replaces template with song specifics.
genJava :: Track -> Float -> String -> String -> String
genJava track bpm name template =
  (replace "%name%"       $ name)                  $
  (replace "%effect%"     $ javaEffects track)     $
  (replace "%bpm%"        $ show bpm)              $
  (replace "%instrument%" $ javaInstruments track) $
  (replace "%pattern%"    $ javaPattern     track) template

-- String containing all java calls to add effects to each instrument.
javaEffects :: Track -> String
javaEffects = (genEffects "") . instrumentDictionary
  where
    genEffects :: String -> [(EInstrument, Int)] -> String
    genEffects calls []     = calls
    genEffects calls (x:xs) = genEffects (call ++ calls) xs
      where
        call = callFromEffectList (snd x) $ (fst . fst) x

-- Java-readable array of strings with sample paths.
javaInstruments :: Track -> String
javaInstruments track =
  "{\"" ++ (concat . (intersperse "\", \"") . listOfSamples) track ++ "\"}"

-- Java-readable instrument hit pattern.
javaPattern :: Track -> String
javaPattern track =
  map replace $ show patterns
  where
    -- Replace Haskell list brackets for Java array brackets.
    replace :: Char -> Char
    replace '[' = '{'
    replace ']' = '}'
    replace chr = chr
    -- Translated sample patterns.
    patterns = map translate $ listOfBeats track
    -- Translate sample names into array indexes.
    translate :: [EInstrument] -> [Int]
    translate [] = []
    translate (x:xs) =
      case lookup x dictionary of
        Just hit -> hit : translate xs
        Nothing  -> []
    dictionary = instrumentDictionary track

-- Dictionary of instruments to their Java array indexes.
instrumentDictionary :: Track -> [(EInstrument, Int)]
instrumentDictionary = (f 0 []) . listOfInstruments
  where
    -- Give each instrument an unique array id.
    f :: Int -> [(EInstrument, Int)] -> [EInstrument] -> [(EInstrument, Int)]
    f i acc []     = acc
    f i acc (x:xs) = f (i + 1) ((x, i) : acc) xs

-- List of sample names in a song.
listOfSamples :: Track -> [Instrument]
listOfSamples = (map snd) . listOfInstruments

-- List of each instrument in a song.
listOfInstruments :: Track -> [EInstrument]
listOfInstruments =  nub . concat . listOfBeats
