module JSynCompiler where

import Data.List
--import Data.List.Utils
import Utils
import System.IO

import HMusic

-- TODO: Make sure the number of channels in the backend is equivalent
-- to the number of tracks and not number of instruments in a song.

test = do
  compileJSyn track1 120.0 "./" "Track"

compileJSyn :: Track -> Float -> String -> String -> IO ()
compileJSyn track bpm path name = do
  template <- songTemplate
  let song = genJSyn track bpm name template
  writeFile (path ++ name ++ ".java") song

-- Java class template for a compiled song.
songTemplate :: IO String
songTemplate = do
  handle <- openFile "Template.java" ReadMode
  contents <- hGetContents handle
  return contents

-- Replaces template with song specifics.
genJSyn :: Track -> Float -> String -> String -> String
genJSyn track bpm name template =
  (replace "%name%"       $ name)                  $
  (replace "%bpm%"        $ show bpm)              $
  (replace "%instrument%" $ javaInstruments track) $
  (replace "%pattern%"    $ javaPattern     track) template

-- Java-readable array of strings with sample paths.
javaInstruments :: Track -> String
javaInstruments track =
  "{\"" ++ (concat . (intersperse "\", \"") . listOfInstruments) track ++ "\"}"

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
    -- TODO: Learn about simplifiable class constraints and fix me.
    translate :: [Instrument] -> [Int]
    translate [] = []
    translate lst@(x:xs) =
      case lookup x dictionary of
        Just hit -> hit : translate xs
        Nothing  -> []
    dictionary = instrumentDictionary track

-- Dictionary of instruments to their Java array indexes.
instrumentDictionary :: Track -> [(Instrument, Int)]
instrumentDictionary = (f 0 []) . listOfInstruments
  where
    -- Give each instrument an unique array id.
    f :: Int -> [(Instrument, Int)] -> [Instrument] -> [(Instrument, Int)]
    f i acc []     = acc
    f i acc (x:xs) = f (i + 1) ((x, i) : acc) xs

-- List of each instrument in a song.
listOfInstruments :: Track -> [Instrument]
listOfInstruments = nub . concat . listOfBeats
