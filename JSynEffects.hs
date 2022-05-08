module JSynEffects where

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
  (replace "%effect%" $ jSynAmp)  $
  (replace "%idx%"    $ show idx) $ jSynEffect

jSynEffect = ".attachEffect(%idx%, new Function() %effect%)"

jSynAmp = "{\
          \ double arg;\
          
          \ public Function withArg (double x)\
          \ {\
          \ arg = x;\
          
          \ return this;\
          \ } \
          
          \ public double evaluate (double x)\
          \ {\
          \ return arg * x;\
          \ }\
          \ }.withArg(%arg%)"
