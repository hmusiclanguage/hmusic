module HMusic where
import System.Cmd
import Data.List
import System.IO
import Data.IORef
import System.IO.Unsafe


data MPattern = X | O | MPattern :|  MPattern
    deriving (Eq,Show)
data Track = MakeTrack Instrument MPattern 
            | MakeTrackE Instrument [Effect] MPattern 
            | Track :|| Track
            | Master [Effect] Track 
            | MasterN String [Effect] Track
    deriving (Eq, Show)

data Effect = Reverb Float | Amp Float | Attack Float | Release Float | Rate Float | Sustain Float
		| Start Float | Finish Float | Echo
    deriving (Eq,Show)

type Instrument = String


sonicPiToolPath :: String
sonicPiToolPath = "/home/andre/.cargo/bin/"

infixr  7  :|
infixr  6  :||



(.*) :: Int -> MPattern -> MPattern
1 .* p = p
n .* p = p :| (n-1) .* p

hihatVerse :: MPattern
hihatVerse = 8 .* (X :| O :| X :| O)

hihatChorus :: MPattern
hihatChorus = 4 .* (X :| X :| X :| X)  

hihatSong :: MPattern
hihatSong = hihatVerse :| hihatChorus :| hihatVerse :| hihatChorus 

hihatSong2 :: MPattern
hihatSong2 = 2 .* (hihatVerse :| hihatChorus)

--- glues two tracks (or multi-tracks), one after the other
(|+) :: Track -> Track -> Track
t1 |+ t2 = sumTracks t1 t2


sumTracks :: Track -> Track -> Track
sumTracks t1 t2 = 
  let sizet1 = lengthTrack t1;
  in let sizet2 = lengthTrack t2
     in case separateTracks t1 t2 of
         (Nothing, Nothing, [])  -> error "Fatal Error: Impossible, all tracks are empty!"
         (Nothing, Nothing, l)  -> joinTracks sizet1 l
         (Nothing, Just tr2, []) -> addSilenceToBegin sizet1 tr2
         (Nothing, Just tr2, l) -> addSilenceToBegin sizet1 tr2 :|| joinTracks sizet1 l
         (Just tr1, Nothing, []) -> tr1
         (Just tr1, Nothing, l) -> tr1 :|| joinTracks sizet1 l
         (Just tr1, Just tr2,[]) -> tr1 :|| addSilenceToBegin sizet1 tr2
         (Just tr1, Just tr2,l) ->  tr1 :|| addSilenceToBegin sizet1 tr2 :|| joinTracks sizet1 l
        

joinTracks :: Int -> [(Track,Track)] -> Track
joinTracks n [] = error "Empty list!"
joinTracks n [x] = joinTrack n x
joinTracks n (x:xs) = joinTrack n x :|| joinTracks n xs

joinTrack :: Int -> (Track,Track) -> Track
joinTrack n (MakeTrack i1 dp1,MakeTrack i2 dp2) = 
  let sizedp = lengthDP dp1
  in  if (sizedp < n)
      then MakeTrack i1 (dp1 :| genSilence (n-sizedp) :| dp2)
      else MakeTrack i1 (dp1 :| dp2)
joinTrack n (MakeTrackE i1 e1 dp1,MakeTrackE i2 e2 dp2) = 
      let sizedp = lengthDP dp1
      in  if (sizedp < n)
          then MakeTrackE i1 e1 (dp1 :| genSilence (n-sizedp) :| dp2)
          else MakeTrackE i1 e1 (dp1 :| dp2)
joinTrack n (MakeTrack i1 dp1,MakeTrackE i2 [] dp2) = 
      let sizedp = lengthDP dp1
      in  if (sizedp < n)
          then MakeTrackE i1 [] (dp1 :| genSilence (n-sizedp) :| dp2)
          else MakeTrackE i1 [] (dp1 :| dp2)
joinTrack n (MakeTrackE i1 [] dp1,MakeTrack i2 dp2) = 
      let sizedp = lengthDP dp1
      in  if (sizedp < n)
          then MakeTrackE i1 [] (dp1 :| genSilence (n-sizedp) :| dp2)
          else MakeTrackE i1 [] (dp1 :| dp2)
joinTrack n (Master e1 t1,Master e2 t2) = Master e1 (joinTrack n (t1, t2))
joinTrack n (MasterN n1 e1 t1,MasterN n2 e2 t2) = MasterN n1 (e1++e2) (sumTracks t1 t2)
joinTrack n (t11:||t12,t21:||t22) = joinTrack n (t11, t21) :|| joinTrack n (t12, t22)

addSilenceToBegin :: Int -> Track -> Track
addSilenceToBegin n (MakeTrack i dp) =  MakeTrack i (genSilence n :| dp)
addSilenceToBegin n (MakeTrackE i e dp) =  MakeTrackE i e (genSilence n :| dp)
--addSilenceToBegin n (t@(MakeTrack i dp) :|| ts) = 
--   MakeTrack i (genSilence n :| dp) :|| addSilenceToBegin n ts
addSilenceToBegin n (Master e t) = Master e (addSilenceToBegin n t)
addSilenceToBegin n (MasterN na e t) = MasterN na e (addSilenceToBegin n t)
addSilenceToBegin n (t1 :|| t2) = (addSilenceToBegin n t1) :|| (addSilenceToBegin n t2)

	
addSilenceToTheEnd :: Int -> Track -> Track
addSilenceToTheEnd n  t@(MakeTrack i dp) = 
  let sizedp = lengthDP dp 
  in if (sizedp<n)
     then MakeTrack i (dp :| genSilence (n-sizedp))
   else t
addSilenceToTheEnd n  t@(MakeTrackE i e dp) = 
    let sizedp = lengthDP dp 
    in if (sizedp<n)
       then MakeTrackE i e (dp :| genSilence (n-sizedp))
     else t   
--addSilenceToTheEnd n (t@(MakeTrack i dp) :|| ts) =
-- let sizedp = lengthDP dp 
-- in if (sizedp<n)
--     then ((MakeTrack i (dp :| genSilence (n-sizedp))) :|| addSilenceToTheEnd n ts)
--	 else (t :|| addSilenceToTheEnd n ts)
addSilenceToTheEnd n (Master e t) = Master e (addSilenceToTheEnd n t)
addSilenceToTheEnd n (MasterN na e t) = MasterN na e (addSilenceToTheEnd n t)
addSilenceToTheEnd n (t1 :|| t2) = (addSilenceToTheEnd n t1) :|| (addSilenceToTheEnd n t1)


----- takes two tracks t1 and t2 and returns a triple, 
-- * First element contains all tracks from t1 that do not have instruments equal
-- to instruments in t2 
-- * Secont element all tracks from t2 that have no matching instruments with tracks
-- from t1 
-- * List of tuples containg matching tracks (t1, t2)

-- Recursion is on the first argument. For all tracks in t1, try to find a matching
-- track in t2
--

separateTracks :: Track -> Track -> (Maybe Track , Maybe Track,  [(Track,Track)])
separateTracks t1@(MakeTrack i dp) t2 = case getTrack t1 t2 of
	Nothing -> (Just t1, Just t2,[])
	Just tr2  -> let (b,mt) = removeTrack t1 t2 
               in (Nothing, mt, [(t1,tr2)])
separateTracks t1@(MakeTrackE i e dp) t2 = case getTrack t1 t2 of
	Nothing -> (Just t1, Just t2,[])
	Just tr2  -> let (b,mt) = removeTrack t1 t2 
	             in (Nothing, mt, [(t1,tr2)])
separateTracks t1@(Master e t) t2 = case getTrack t1 t2 of
     Nothing -> (Just t1, Just t2, [])
     Just tr2 -> let (b,mt) = removeTrack t1 t2 
                 in (Nothing, mt, [(t1,tr2)])
separateTracks t1@(MasterN n e t) t2 = case getTrack t1 t2 of
     Nothing -> (Just t1, Just t2, [])
     Just tr2 -> let (b,mt) = removeTrack t1 t2 
                 in (Nothing, mt, [(t1,tr2)])
separateTracks (ta :|| tb) t2 = case separateTracks ta t2 of
	(tr1,tr2,list) -> case removeListOfTracks list t2 of
						Nothing -> (joinMaybeTracks tr1 (Just tb), tr2,list)
						Just ntr2 -> let (a,b,c) = separateTracks tb ntr2
						             in (joinMaybeTracks tr1 a, b, list ++c)

joinMaybeTracks :: Maybe Track -> Maybe Track -> Maybe Track
joinMaybeTracks Nothing a = a
joinMaybeTracks t@(Just tr) Nothing = t
joinMaybeTracks (Just tr1) (Just tr2) = Just (tr1 :|| tr2)

--
removeListOfTracks :: [(Track,Track)] -> Track -> Maybe Track
removeListOfTracks [] t = Just t
removeListOfTracks ((t1,_):xs) t2 = case removeTrack t1 t2 of	
      (False,_) -> removeListOfTracks xs t2
      (True,Nothing) -> Nothing
      (True,Just t) -> removeListOfTracks xs t 

---removeListOfTracks ((t@(MakeTrackE i e p),_):xs) t2 = case removeTrack t t2 of	
---			(False,_) -> removeListOfTracks xs t2
---			(True,Nothing) -> Nothing
---			(True,Just t) -> removeListOfTracks xs t




--- 
--- Takes a base case track t1 and a possibly multi-track t2
--- and returns a track from an equivalent track from t2.
--- The notion of equivalence is defined in the sameTrack function:
-- if t2 has the same instrument
--- as t1 (simple track), same instrument and  same effects (in the case
--  of a track with effects), 
---- and in the case of a Master, same effects and same track using
--- the equivalence notion expressed above

getTrack :: Track -> Track -> Maybe Track
getTrack t1 t2@(MakeTrack i2 dp2)
    | sameTrack t1 t2 = Just t2
    | otherwise = Nothing
getTrack t1 t2@(MakeTrackE i2 e2 dp2)
    | sameTrack t1 t2 = Just t2
    | otherwise = Nothing
getTrack t1 t2@(Master _ _)
    | sameTrack t1 t2  = Just t2
    | otherwise = Nothing
getTrack t1 t2@(MasterN _ _ _)
    | sameTrack t1 t2  = Just t2
    | otherwise = Nothing
getTrack i1 (t1 :|| t2) = case getTrack i1 t1 of	
    Just t -> Just t
    Nothing -> getTrack i1 t2




{- getTrack :: Track -> Track -> Maybe Track
getTrack (MakeTrack i1 dp1) t@(MakeTrack i2 dp2)
    | i1 == i2 = Just t
    | otherwise = Nothing
getTrack (MakeTrackE i1 e1 dp1) t@(MakeTrackE i2 e2 dp2)
    | i1 == i2 && e1 == e2 = Just t
    | otherwise = Nothing
getTrack (MakeTrackE i1 e1 dp1) t@(MakeTrack i2 dp2)
    | i1 == i2 && e1 == [] = Just t
    | otherwise = Nothing
getTrack (MakeTrack i1 dp1) t@(MakeTrackE i2 e dp2)
    | i1 == i2 && e == [] = Just t
    | otherwise = Nothing
getTrack t1@(Master _ _) t2@(Master _ _)
    | sameTrack t1 t2  = Just t2
    | otherwise = Nothing
getTrack i1 (t1 :|| t2) = case getTrack i1 t1 of	
							Just t -> Just t
							Nothing -> getTrack i1 t2

 -}
removeTrack :: Track -> Track -> (Bool,Maybe Track)
removeTrack t1@(MakeTrack i1 dp1) t2@(MakeTrack i2 dp2)
    | i1 == i2 = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack t1@(MakeTrackE i1 e dp1) t2@(MakeTrack i2 dp2)
    | i1 == i2 && e== [] = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack t1@(MakeTrackE i1 e1 dp1) t2@(MakeTrackE i2 e2 dp2)
    | i1 == i2 && e1==e2 = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack t1@(MakeTrack i1 dp1) t2@(MakeTrackE i2 e dp2)
    | i1 == i2 && e== [] = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack t1@(Master _ _) t2@(Master _ _)
    | sameTrack t1 t2  = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack t1@(MasterN _ _ _) t2@(MasterN _ _ _)
    | sameTrack t1 t2  = (True,Nothing)
    | otherwise = (False,Just t2)
removeTrack i (t1 :|| t2) = case removeTrack i t1 of
					(True,Nothing) -> (True, Just t2)
					(True, Just tr) -> (True, Just (tr :|| t2) )
					(False, _) -> case removeTrack i t2	of
								(True, Nothing)-> (True, Just t1)
								(True, Just tr) -> (True , Just (t1 :|| tr))
								(False, _) -> (False, Just (t1:||t2))



							
							
{-
removeFstTrack :: Instrument -> Track -> Maybe Track
removeFstTrack i1 t@(MakeTrack i2 dp1)
	| i1 == i2 = Nothing
    | otherwise = Just t
removeFstTrack i1 (t@(MakeTrack i2 dp1) :|| ts)
	| i1 == i2 =  Just ts
    | otherwise = case removeTrack i1 ts of
					Nothing ->  Just t
					Just tr -> Just (t:|| tr) 
					(False, _) -> (False, Just (t:||ts))
removeTrack i (t1 :|| t2) = case removeTrack i t1 of
					(True,Nothing) -> (True, Just t2)
					(True, Just tr) -> (True, Just (tr :|| t2) )
					(False, _) -> case removeTrack i t2	of
								(True, Nothing)-> (True, Just t1)
								(True, Just tr) -> (True , Just (t1 :|| tr))
								(False, _) -> (False, Just (t1:||t2))
							
							
							
removeTrack :: Instrument -> Track -> (Bool,Maybe Track)
removeTrack i1 t@(MakeTrack i2 dp1)
	| i1 == i2 = (True,Nothing)
    | otherwise = (False,Just t)
removeTrack i1 (t@(MakeTrack i2 dp1) :|| ts)
	| i1 == i2 = (True, Just ts)
    | otherwise = case removeTrack i1 ts of
					(True,Nothing) -> (True, Just t)
					(True, Just tr) -> (True, Just (t:|| tr) )
					(False, _) -> (False, Just (t:||ts))
removeTrack i (t1 :|| t2) = case removeTrack i t1 of
					(True,Nothing) -> (True, Just t2)
					(True, Just tr) -> (True, Just (tr :|| t2) )
					(False, _) -> case removeTrack i t2	of
								(True, Nothing)-> (True, Just t1)
								(True, Just tr) -> (True , Just (t1 :|| tr))
								(False, _) -> (False, Just (t1:||t2))



							
combineTracks :: Int -> Int -> Track -> Track -> Track
combineTracks size1 size2 t@(MakeTrack i1 dp1) t2 = insertTrack size1 size2 t t2
combineTracks size1 size2 (t@(MakeTrack i1 dp1) :|| ts) t2 = combineTracks size1 size2 ts (insertTrack t t2)
combineTracks size1 size2 (t1 :|| t2) t3 =   combineTracks size1 size2 ((tailTr t1) :|| t2) (combineTracks size1 size2 (headTr t1) t3)

insertTrack sizetr1 sizetr2 (MakeTrack i dp1) (MakeTrack i2 dp2)
	| i == i2 = let sizedp1 = lengthDP dp1
				in 	if (sizedp1 < sizetr1)
					then MakeTrack i ((dp1 :| genSilence (sizetr1 - sizedp)) :| dp2)
					else MakeTrack i (dp1 :| dp2)
	|otherwise = MakeTrack i dp1 :||  MakeTrack i2 dp2

-}

-- Definition of "same track" used for track composition:
-- Tracks are the same if they have the same Instrument, i.e.,
-- play the same sample


sameTrack :: Track -> Track -> Bool
sameTrack (MakeTrack i1 p1) (MakeTrack i2 p2)
          | i1 == i2 = True
          | otherwise = False
sameTrack (MakeTrackE i1 e1 p1) (MakeTrackE i2 e2 p2)
          | i1 == i2 && e1 == e2 = True
          | otherwise = False
sameTrack (Master e1 t1) (Master e2 t2)
          | e1 == e2 = sameTrack t1 t2
          | otherwise = False
sameTrack (MasterN n1 e1 t1) (MasterN n2 e2 t2)
          | n1 == n2 = True
          | otherwise = False
sameTrack (MakeTrackE i1 e1 dp1) t@(MakeTrack i2 dp2)
          | i1 == i2 && e1 == [] = True
          | otherwise = False
sameTrack (MakeTrack i1 dp1) t@(MakeTrackE i2 e dp2)
          | i1 == i2 && e == [] = True
          | otherwise = False
sameTrack (MakeTrack i1 p1) t = False
sameTrack (MakeTrackE i1 e1 p1) t = False
sameTrack (Master e1 t1) t = False 
sameTrack (MasterN n1 e1 t1) t = False 
sameTrack (t11 :|| t12) (t21 :|| t22) = sameTrack t11 t21 && sameTrack t12 t22
sameTrack (t11 :|| t12) t = False


samePattern :: MPattern -> MPattern -> Bool
samePattern X X = True
samePattern O O = True
samePattern X p = False
samePattern O p = False
samePattern (p11 :| p12) (p21 :| p22) = samePattern p11 p21 && samePattern p12 p22



headTr :: Track -> Track
headTr t@(MakeTrack i p)  = t
headTr t@(MakeTrackE i e p)  = t
headTr t@(Master e tr) = t 
--headTr (t@(MakeTrack i p) :|| p2) = t
--headTr (t@(MakeTrackE i e p) :|| p2) = t
headTr (p1 :|| p2) = headTr p1

tailTr :: Track -> Track
tailTr (MakeTrack i p)  = error "tail of a single Track"
tailTr (MakeTrackE i e p)  = error "tail of a single Track"
tailTr (Master e t) = error "tail of a single Track"
--tailTr (MakeTrack i p :|| p2) = p2
--tailTr (MakeTrackE i e p :|| p2) = p2
tailTr (p1 :|| p2) = tailTr p1

-- repeats a multi track n times
(|*) :: Int -> Track -> Track
n |* t = multTrack (lengthTrack t) n t


-- insertTrack :: Int -> Track -> Track -> Track
--insertTrack size (MakeTrack i dp) (MakeTrack i2 dp2)
--	| i == i2 = let sizedp = lengthDP dp 
--                in case (sizedp < size) of
--				 True -> MakeTrack i ((dp2 :| genSilence (size - sizedp)) :| dp)
--				 False -> MakeTrack i (dp2 :| dp)
--	|otherwise = MakeTrack i2 dp2 :||  (MakeTrack i  ((genSilence size):| dp))

--- THIS IS WRONG, WE NEED CASE T1 :|| T2

multTrack :: Int -> Int -> Track -> Track
multTrack size n (MakeTrack i dp) = let sizedp = lengthDP dp 
                                    in  case (sizedp < size) of
                                          True -> MakeTrack i (n .* (dp :| genSilence (size - sizedp)))
                                          False -> MakeTrack i (n .* dp)
multTrack size n (MakeTrackE i e dp) = let sizedp = lengthDP dp 
                                    in  case (sizedp < size) of
                                          True -> MakeTrackE i e (n .* (dp :| genSilence (size - sizedp)))
                                          False -> MakeTrackE i e (n .* dp)
multTrack size n (Master e t) = Master e (multTrack size n t)
multTrack size n (MasterN na e t) = MasterN na e (multTrack size n t)
multTrack size n (t1 :|| t2) = multTrack size n t1 :|| multTrack size n t2

{-
multTrack size n ((MakeTrack i dp):|| t) = let sizedp = lengthDP dp 
                                           in  case (sizedp < size) of
                                                True -> (MakeTrack i (n .* (dp :| genSilence (size - sizedp)))) :|| multTrack size n t
                                                False -> (MakeTrack i (n .* dp)) :|| multTrack size n t
multTrack size n ((MakeTrackE i e dp):|| t) = let sizedp = lengthDP dp 
                                           in  case (sizedp < size) of
                                                True -> (MakeTrackE i e (n .* (dp :| genSilence (size - sizedp)))) :|| multTrack size n t
                                                False -> (MakeTrackE i e (n .* dp)) :|| multTrack size n t

-} 

genSilence :: Int -> MPattern
genSilence n = takeBeats n infiniteSilence
	where	infiniteSilence = O :| infiniteSilence

-----------------------
----- The clapping music by Steve Reich
-----------------------------

clappingPat :: MPattern
clappingPat = X :| X :| X :| O :| X :| X :| O :| X :| O :| X :| X :| O




shiftPat :: MPattern -> MPattern
shiftPat O = O
shiftPat X = X
shiftPat (O :| p) = p :| O
shiftPat (X :| p) = p :| X
shiftPat (p1 :| p2) = (tailDP p1) :| p2 :| (headDP p1)

fstPatternCS :: MPattern
fstPatternCS = 104 .* clappingPat

sndPatternCS :: MPattern
sndPatternCS = 8.* clappingPat :| shiftMany 12 8 clappingPat 

clappingSong :: Track
clappingSong =      MakeTrack "AcousticSnare" fstPatternCS
		:|| MakeTrack "HandClap" sndPatternCS

shiftMany :: Int -> Int -> MPattern -> MPattern
shiftMany 1 t p = t .* (shiftPat p)
shiftMany n t p = t .* shifted :| shiftMany (n-1) t shifted 
                  where shifted = shiftPat p 
		

clapMusicPat :: MPattern
clapMusicPat = O

kick :: MPattern
kick =  X :| O :| O :| O 

snare = O :| O :| X :| O

-----------------------------------------
----------------- composition examples for paper

track1 =  
  MakeTrack "drum_bass_hard"          (X)
  :|| MakeTrack "drum_snare_hard"     (O :| O :| X)
  :|| MakeTrack "drum_cymbal_closed"  (X :| X :| X :| X)


t1  =  
  MakeTrack "drum_bass_hard"          (X :| X)
  :|| MakeTrack "drum_snare_hard"     (O :| O :| X :| O)

te1 =  
    MakeTrack "drum_bass_hard"          (X)
    :|| MakeTrackE "drum_snare_hard"  [Reverb 1.0, Amp 1.0]   (O :| O :| X)
    :|| MakeTrack "drum_cymbal_closed"  (X :| X :| X :| X)
  
te2  =  
    MakeTrackE "drum_bass_hard" [Reverb 1.0]         (X :| X)
    :|| MakeTrack "drum_snare_hard"     (O :| O :| X :| O)  

tm1 = MakeTrack "drum_bass_hard" X
    :|| Master [Reverb 1.0] te2
  
t2 =  MakeTrack "drum_cymbal_closed"  (X :| X :| X :| X)

t3 = MakeTrack "ambi_choir" X

track2 =  MakeTrack "BassDrum"  (X :| O :| O :| O)
  :|| MakeTrack "AcousticSnare" (O :| O :| X :| O)
  :|| MakeTrack "ClosedHiHat"   (X :| O :| X )
  :|| MakeTrack "Cowbell"       (X)

trackcomp =  
 MakeTrack "kick"                                               X
 :|| MakeTrackE "snare" [Reverb 0.5]                           (O :| O :| X)
 :|| MakeTrackE "hihat" [Attack 0.1, Sustain 0.3, Release 0.1] (X :| X :| X :| X)
 :|| MakeTrack "guitar"                                         X

track =  
 MakeTrack "kick"       X
 :|| MakeTrack "snare" (O :| O :| X)
 :|| MakeTrack "hihat" (X :| X :| X :| X)
 :|| MakeTrack "Guitar" X

subsInstrument :: Instrument -> Instrument -> Track -> Track
subsInstrument i1 i2 (MakeTrack i p)
   | i == i1 = MakeTrack i2 p
   | otherwise = (MakeTrack i p)
subsInstrument i1 i2 (t1 :|| t2) = subsInstrument i1 i2 t1 :|| subsInstrument i1 i2 t2



{-      
track2 =  MakeTrack BassDrum  (X :| O :| O :| O)
  :|| MakeTrack AcousticSnare (O :| O :| X :| O)
  :|| MakeTrack ClosedHiHat   (X :| O :| X )
  :|| MakeTrack Cowbell       (X)
  
track1track2 = 
 MakeTrack BassDrum          (X :| O :| O :| O :| X :| O :| O :| O)
 :|| MakeTrack AcousticSnare (O :| O :| X :| O :| O :| O :| X :| O)
 :|| MakeTrack ClosedHiHat   (X :| X :| X :| X :| X :| O :| X )
 :|| MakeTrack Cowbell       (O :| O :| O :| O :| X )
 
track2track1 = 
 MakeTrack BassDrum          (X :| O :| O :| O :| X)
 :|| MakeTrack AcousticSnare (O :| O :| X :| O :| O :| O :| X :| O)
 :|| MakeTrack ClosedHiHat   (X :| O :| X :| O :| X :| X :| X :| X)
 :|| MakeTrack Cowbell       (X)

track1twice =  
  MakeTrack BassDrum          (X :| O :| O :| O :| X)
  :|| MakeTrack AcousticSnare (O :| O :| X :| O :| O :| O :| X )
  :|| MakeTrack ClosedHiHat   (X :| X :| X :| X :| X :| X :| X :| X)
  
track2twice = 
  MakeTrack BassDrum          (X :| O :| O :| O :| X :| O :| O :| O)
  :|| MakeTrack AcousticSnare (O :| O :| X :| O :| O :| O :| X :| O)
  :|| MakeTrack ClosedHiHat   (X :| O :| X :| O :| X :| O :| X)
  :|| MakeTrack Cowbell       (X :| O :| O :| O :| X)
--------------------------

walkThisWay = 
  MakeTrack ClosedHiHat       (O :| O :| O :| O :| X :| O :| O :| X :| X :| O :| X :| O :| X :| O :| O :| O)
  :|| MakeTrack CrashCymbal1   X
  :|| MakeTrack AcousticSnare (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
  :|| MakeTrack BassDrum      (X :| O :| O :| O :| O :| O :| O :| X :| X :| O :| X :| O :| O )

ww = 8 |* walkThisWay

reggaeSong =  MakeTrack CrashCymbal1 (O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
  :|| MakeTrack ClosedHiHat          (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O)
  :|| MakeTrack Tambourine           (X :| O :| X :| X :| O :| X :| O :| X :| O :| X :| X :| O :| X :| X :| O :| X)          
  :|| MakeTrack AcousticSnare        (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X)
  :|| MakeTrack BassDrum             (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X)

reaggae = 8 |* reggaeSong
  
bossaNova = MakeTrack SideStick      (X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X :| X)          
  :|| MakeTrack AcousticSnare        (X :| O :| O :| X :| O :| O :| X :| O :| O :| O :| X :| O :| O :| X :| O :| O)
  :|| MakeTrack BassDrum             (X :| O :| O :| X :| X :| O :| O :| X :| X :| O :| O :| X :| X :| O :| O :| X)
  
bn = 8 |* bossaNova


drumAndBass1 =
      MakeTrack ClosedHiHat   (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| X :| X :| X :| X :| O)
  :|| MakeTrack AcousticSnare (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
  :|| MakeTrack BassDrum      (X :| O :| O :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O )

db1 = 8 |* drumAndBass1

drumAndBass2 = 
   MakeTrack OpenHiHat        (O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| X :| X :| O :| X :| O :| X)
  :|| MakeTrack ClosedHiHat   (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O)
  :|| MakeTrack CrashCymbal1  (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O)
  :|| MakeTrack AcousticSnare (O :| X :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O :| X :| X :| X :| X)
  :|| MakeTrack BassDrum      (X :| O :| O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O )

db2 = 8 |* drumAndBass2

myTrack :: Track
myTrack =
      MakeTrack BassDrum      ( X :| O :| O :| O :| O :| O :| O :| X )
 :||  MakeTrack AcousticSnare ( X :| O :| O :| O :| O :| O :| O :| O )
  



sexualHealing :: Track 
sexualHealing =      
	   MakeTrack BassDrum  ( X :| O :| O :| O :| O :| O :| O :| X :| X :| O :| X :| O :| X:| O :| O :| X ) 
  :||  MakeTrack AcousticSnare ( X :| O :| O :| O :| O :| O :| O :| X )
  
 
rock:: Track
rock = 
		MakeTrack ClosedHiHat (3 .* ( X :| X :| X :| X :| X :| X :| X :| X ))
 :||	MakeTrack AcousticSnare       (3 .* ( O :| O :| X :| O :| O :| O :| X :| O ))
 :||  	MakeTrack BassDrum            (3 .* ( X :| O :| O :| X :| X :| O :| O :| O ))

 -}


drumAndBass2 = 
  MakeTrack "drum_cymbal_open"        (O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| X :| X :| O :| X :| O :| X)
 :|| MakeTrack "drum_cymbal_closed"   (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O)
 :|| MakeTrack "drum_cymbal_hard"     (X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O :| X :| O)
 :|| MakeTrack "drum_snare_hard"      (O :| X :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O :| X :| X :| X :| X)
 :|| MakeTrack "drum_bass_hard"       (X :| O :| O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O )

drumAndBass1 =
   MakeTrack "drum_snare_hard"  (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
  :|| MakeTrack "drum_bass_hard"   (X :| O :| O :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O )

walkThisWay = 
    MakeTrack "drum_cymbal_closed"       (O :| O :| O :| O :| X :| O :| O :| X :| X :| O :| X :| O :| X :| O :| O :| O)
    :|| MakeTrack "drum_cymbal_hard"   X
    :|| MakeTrack "drum_snare_hard" (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
    :|| MakeTrack  "drum_bass_hard"     (X :| O :| O :| O :| O :| O :| O :| X :| X :| O :| X :| O :| O )
  



trackD =MakeTrack  "ambi_drone"    (X :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O :| O)

sacomp = 
    MakeTrack "drum_snare_hard" (O :| O :| O :| O :| X :| O :| O :| O :| O :| O :| O :| O :| X :| O :| O :| O)
    :|| MakeTrack  "drum_bass_hard"     (X :| O :| O :| O :| O :| O :| O :| X :| X :| O :| X :| O :| O )

cymbal =  MakeTrack "drum_cymbal_closed"   (X :| O :| X :| O :| X :| X :| X :| X :| X :| X :| X :| X :| O :| X :| O :| X)
  
flute = MakeTrack ":flute.wav" (X :| genSilence 15)
        :|| MakeTrack ":flute.wav" (X :| genSilence 15)

alo = MakeTrack ":alo.wav" (X:|genSilence 15)   :|| MakeTrack ":alo.wav" (X:|genSilence 15) 

tough = MakeTrack ":tough.wav" (X :| genSilence 15) :|| MakeTrack ":tough.wav" (X :| genSilence 15)

quebradera = (auau  :|| sacomp :|| alo)

auau = MakeTrack ":auau.wav" (genSilence 15 :| X) :|| MakeTrack ":auau.wav" (genSilence 15 :| X)

final = (2 |* sacomp) :|| alo :|| auau

lengthmp :: MPattern -> Int
lengthmp (x:|y) = lengthmp x + 
                   lengthmp y 
lengthmp _ = 1

mapTrack :: (Track -> Track) -> Track -> Track
mapTrack f (t1 :|| t2) = mapTrack f t1 :|| mapTrack f t2
mapTrack f t = f t

drumsN :: Track
drumsN 
   = MasterN "drums" [Amp 0.2] te1


subsInstr :: Instrument -> Instrument -> 
    Track -> Track
subsInstr i1 i2 t@(MakeTrack i p)
  | i == i1 = MakeTrack i2 p
  | otherwise = t
subsInstr i1 i2 t = t


changeMaster :: String -> [Effect] -> Track -> Track
changeMaster name e t@(MasterN n em tm) 
  | name == n = MasterN n e tm
  | otherwise = t
changeMaster i e t = t

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

subsEffects :: [Effect] -> [Effect] -> Track -> Track
subsEffects e1 e2 t@(MakeTrackE i e f)
  | e1 == e = MakeTrackE i e2 f
  | otherwise = t
subsEffects e1 e2 t = t

changeTrack :: Instrument -> [Effect] -> Track -> Track
changeTrack i e t@(MakeTrack it p) 
  | i == it = MakeTrackE i e p
  | otherwise = t
changeTrack i e (t1:||t2) = changeTrack i e t1 :|| changeTrack i e t2

lengthDP :: MPattern -> Int
lengthDP O = 1
lengthDP X = 1
lengthDP (X:|p) = 1 + lengthDP p
lengthDP (O:|p) = 1 + lengthDP p
lengthDP (x:|y) = lengthDP x + lengthDP y 


lengthTrack :: Track -> Int
lengthTrack (MakeTrack _ dp) = lengthDP dp
lengthTrack (MakeTrackE _ e dp) = lengthDP dp
lengthTrack (Master e t) = lengthTrack t
lengthTrack (MasterN n e t) = lengthTrack t
lengthTrack (t1 :|| t2) = max (lengthTrack t1) (lengthTrack t2)

headDP :: MPattern -> MPattern
headDP X = X
headDP O = O
headDP (X:|p) = X
headDP (O:|p) = O
headDP (x:|y) = headDP x

tailDP :: MPattern -> MPattern
tailDP X = error "tail of a one beat drum pattern"
tailDP O = error "tail of a one beat drum pattern"
tailDP (X:|p) = p
tailDP (O:|p) = p
tailDP (x:|y) = (tailDP x) :| y

takeBeats :: Int -> MPattern -> MPattern
takeBeats n dp
   | n<1 = error "takeBeats n, where n is less than zero"
   | otherwise = let (dpf,_) = takeBeatsT n dp in dpf
 

 
takeBeatsT :: Int -> MPattern -> (MPattern, Int)
takeBeatsT 1 dp = (headDP dp, 0)
takeBeatsT n X = (X,n-1)
takeBeatsT n O = (O,n-1)
takeBeatsT n (X:|p) = let (dp,r) = takeBeatsT (n-1) p in (X:|dp,r)
takeBeatsT n (O:|p) = let (dp,r) = takeBeatsT (n-1) p in (O:|dp,r)
takeBeatsT n (x:|y) = let (db,r) = takeBeatsT n x 
					  in case r>= 1 of
							True -> let (dbf,rf) = takeBeatsT r y in (db:|dbf,rf)
							False -> (db,r)

  

  
-----------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------



getBeat :: Track -> [(Maybe [Effect], Instrument)]
getBeat (MakeTrack i p)
    | isAHit p = [(Nothing,i)]
    | otherwise = []
getBeat (MakeTrackE i e p)
    | isAHit p = [(Just e,i)]
    | otherwise = []
getBeat (Master e t) = map (addEffects e) (getBeat t)
  where addEffects :: [Effect] -> (Maybe [Effect], Instrument) -> (Maybe [Effect], Instrument)
        addEffects e (Nothing,i) = (Just e, i)
        addEffects e (Just et,i) = (Just (e++et),i)
getBeat (t1 :|| t2) = getBeat t1 ++ getBeat t2
--getBeat ((MakeTrack i dp))




isAHit :: MPattern -> Bool
isAHit O = False
isAHit X = True
isAHit (p1 :| p2) = isAHit p1

removeBeat :: MPattern -> Maybe MPattern
removeBeat O = Nothing
removeBeat X = Nothing
removeBeat (p1 :| p2) = case removeBeat p1 of
    Just np1 ->  Just $ np1 :| p2
    Nothing -> Just p2


removeBeatTrack :: Track -> Maybe Track
removeBeatTrack (MakeTrack i p) = case removeBeat p of
	Nothing -> Nothing
	Just pf -> Just $ MakeTrack i pf
removeBeatTrack (MakeTrackE i e p) = case removeBeat p of
	Nothing -> Nothing
	Just pf -> Just $ MakeTrackE i e pf
removeBeatTrack (Master e t) = case removeBeatTrack t of
	Nothing -> Nothing
	Just tf -> Just $ Master e tf
removeBeatTrack (t1 :|| t2)= case removeBeatTrack t1 of
    Just tf1 -> case removeBeatTrack t2 of
                 Just tf2 -> Just $ tf1 :|| tf2
                 Nothing -> Just tf1 
    Nothing -> removeBeatTrack t2 

listOfBeats :: Track -> [[(Maybe [Effect], Instrument)]]
listOfBeats t = case removeBeatTrack t of
					Just tr -> beat : listOfBeats tr
					Nothing -> [beat]
   where beat = getBeat t


--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
------------------ THE SONIC PI BACK-END -----------------------------------
---------------------------------------------------------------------------


musicState :: IORef (Maybe Track)
musicState = unsafePerformIO (newIORef Nothing)

musicBPM :: IORef (Maybe Float)
musicBPM = unsafePerformIO (newIORef Nothing)

genSonicPI :: Float  -> Track -> String
genSonicPI time track = genSonicPI_ time (listOfBeats track)

genSonicPI_ :: Float -> [[(Maybe [Effect], Instrument)]] -> String
genSonicPI_ time [] = ""
genSonicPI_ time (i:xs) = genNotes i ++ "\tsleep " ++ show time ++ "\n" ++ genSonicPI_ time xs

genNotes :: [(Maybe [Effect], Instrument)] -> String
genNotes [] = ""
genNotes ((Nothing,i):xs)
  | head i == ':' = "\tsample \"/s/" ++ tail i++ "\"\n" ++ genNotes xs
  | otherwise = "\tsample :" ++ i++ "\n" ++ genNotes xs
genNotes ((Just e,i):xs)
  | head i == ':' = genWithEffects (e, i)        ++ genNotes xs
  | otherwise     = genWithEffects (e, ":" ++ i) ++ genNotes xs

-- Sonic Pi string to play an instrument once, with effects.
genWithEffects :: ([Effect], Instrument) -> String
genWithEffects (e, i) = genBlockEffects e
                        ++ "sample " ++ i
                        ++ genInstrumentEffects e ++ "\n"
                        ++ genEnds e
  where
    -- Instrument effects.
    genInstrumentEffects :: [Effect] -> String
    genInstrumentEffects [] = ""
    genInstrumentEffects (e:es)
      | not $ isBlockEffect e =
          ", " ++ effectToString e ++ genInstrumentEffects es
      | otherwise = genInstrumentEffects es
    -- Effect blocks.
    genBlockEffects :: [Effect] -> String
    genBlockEffects [] = ""
    genBlockEffects (e:es)
      | isBlockEffect e =
          "with_fx :" ++ effectToString e ++ " do\n" ++ genBlockEffects es
      | otherwise = genBlockEffects es
    -- Effect block ends.
    genEnds :: [Effect] -> String
    genEnds [] = ""
    genEnds (e:es)
      | isBlockEffect e = "end\n" ++ genEnds es
      | otherwise       = genEnds es

-- Whether effect e is applied to a block of code or to an instrument.
isBlockEffect :: Effect -> Bool
isBlockEffect (Reverb n) = True
isBlockEffect Echo       = True
isBlockEffect _          = False

effectToString :: Effect -> String
effectToString (Reverb n) = "reverb" 
effectToString (Amp n) = "amp: " ++ show n
effectToString (Attack n) = "attack: " ++ show n
effectToString (Release n) = "release: " ++ show n
effectToString (Rate n) = "rate: " ++ show n
effectToString (Sustain n) = "sustain: " ++ show n
effectToString (Start n) = "start: " ++ show n
effectToString (Finish n) = "finish: " ++ show n
effectToString Echo = "echo" 
    

play :: Float -> Track  -> IO ()
play bpm track = do
        let sizeTrack = lengthTrack track
        writeIORef musicState (Just track) 
        
	writeFile "HMusic_temp.rb" $ genSonicPI (60/bpm) track 
	v <- system $ sonicPiToolPath++"sonic-pi-tool eval-file HMusic_temp.rb"
    	print $ show v


loop :: Float -> Track  -> IO ()
loop bpm track = do
        let sizeTrack = lengthTrack track
        writeIORef musicState (Just track)
        writeIORef musicBPM (Just (60/bpm)) 
        
	writeFile "HMusic_temp.rb" $ "live_loop :hmusic do\n" ++ genSonicPI (60/bpm) track ++ "end"
	v <- system $ sonicPiToolPath++"sonic-pi-tool eval-file HMusic_temp.rb"
    	print $ show v

applyToMusic :: (Track -> Track) -> IO ()
applyToMusic ftrack = do
    v <- readIORef musicState
    case v of
        Just t -> do 
                  mbpm <- readIORef musicBPM
                  case mbpm of
                    Just bpm -> do
                     let newTrack = ftrack t
                     --print $ show newTrack
                     writeIORef musicState (Just newTrack)
                     writeFile "HMusic_temp.rb" $ "live_loop :hmusic do\n" ++ genSonicPI bpm newTrack ++ "end"
                     r <-system $ sonicPiToolPath ++ "sonic-pi-tool eval-file HMusic_temp.rb"
                     print $ show r
                    --Nothing -> error "No bpm for the running track" 
        Nothing -> error "No running track to be modified"

startMusicServer :: IO ()
startMusicServer = do
   v <- system $ sonicPiToolPath ++ "sonic-pi-tool start-server"
   print $ show v

stopMusicServer :: IO ()
stopMusicServer = do 
   v <- system $ sonicPiToolPath ++ "sonic-pi-tool stop"
   print $ show v

