module Tests where

import JSynCompiler

import HMusic

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

test = do
  compileJSyn testTrackSameSampleDistinctEffect 300.0 "./" "Track"
