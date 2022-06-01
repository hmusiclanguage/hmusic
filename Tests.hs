module Tests where

import JavaBackend

import HMusic

test = do
  compileJava cumbiaDrum 392.0 "Track"

cumbiaHH = X :| O :| X :| X
cumbiaBD = X :| O :| O :| O
cumbiaCB = O :| O :| X :| O

cumbiaDrum =
  MakeTrack  "bd"            cumbiaBD :||
  MakeTrack  "cb"            cumbiaCB :||
  MakeTrackE "hh" [Amp 0.25] cumbiaHH

-----------------
-- Test tracks --
-----------------

-- 2/3 Polyrhythm
poly2Pattern = X :| O :| O :| X :| O :| O
poly3Pattern = X :| O :| X :| O :| X :| O

-- Used to check if the backend properly handles tracks with the same sample
-- twice, but with a different effect in each occurence.
testTrackSameSampleDistinctEffect =
  MakeTrackE "bd" [Amp 0.25] poly3Pattern :||
  MakeTrack  "bd"            poly2Pattern

-- Used to check how HMusic handles duplicated instruments internally.
testTrackDuplicatedInstrument =
  MakeTrack "bd" poly2Pattern :||
  MakeTrack "bd" poly3Pattern
