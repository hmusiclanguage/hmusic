module JSynEffects where

import Data.List.Utils

import HMusic

-- Gets specifically the first effect from the list; if rewriting to use
-- multiple effects, remove call to head and loop through list instead.
callFromEffectList :: Maybe [Effect] -> String
callFromEffectList Nothing      = ""
callFromEffectList (Just lst) = let effect = head lst
                                in callFromEffect effect

callFromEffect :: Effect -> String
callFromEffect (Amp x) =
  (replace "%arg%"    $ show x)  $
  (replace "%effect%" $ jSynAmp) $ jSynEffect

jSynEffect = ".attachEffect(0, new Function() %effect%)"

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
