module JavaEffects where

import Data.List.Utils

import HMusic

-- Gets specifically the first effect from the list; if rewriting to use
-- multiple effects, remove call to head and loop through list instead.
callFromEffectList :: Int -> Maybe [Effect] -> String
callFromEffectList _ Nothing      = ""
callFromEffectList idx (Just lst) = let effect = head lst
                                    in callFromEffect idx effect

callFromEffect :: Int -> Effect -> String
callFromEffect idx (Amp x) =
  (replace "%arg%"    $ show x)   $
  (replace "%effect%" $ fxAmp)    $
  (replace "%idx%"    $ show idx) $ javaEffect

javaEffect = ".attachEffect(%idx%, %effect%)"

fxAmp = "(x) -> %arg% * x"
