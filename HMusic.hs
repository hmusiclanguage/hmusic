module HMusic where
import System.Cmd
import Data.List
import System.IO
import Data.IORef
import System.IO.Unsafe


data MPattern = X | O | MPattern :|  MPattern
    deriving (Eq,Show)
data Track = MakeTrack Instrument MPattern | MakeTrackE Instrument [Effect] MPattern| Track :|| Track 
    deriving (Eq, Show)

data Effect = Reverb Float | Amp Float
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
joinTracks n [(MakeTrack i1 dp1,MakeTrack i2 dp2)] = 
  let sizedp = lengthDP dp1
  in  if (sizedp < n)
      then MakeTrack i1 (dp1 :| genSilence (n-sizedp) :| dp2)
	  else MakeTrack i1 (dp1 :| dp2)
joinTracks n ((MakeTrack i1 dp1,MakeTrack i2 dp2):xs) = 
  let sizedp = lengthDP dp1
  in  if (sizedp < n)
      then MakeTrack i1 (dp1 :| genSilence (n-sizedp) :| dp2) :|| joinTracks n xs
	  else MakeTrack i1 (dp1 :| dp2) :|| joinTracks n xs
	  

addSilenceToBegin :: Int -> Track -> Track
addSilenceToBegin n  t@(MakeTrack i dp) =  MakeTrack i (genSilence n :| dp)
addSilenceToBegin n (t@(MakeTrack i dp) :|| ts) = 
   MakeTrack i (genSilence n :| dp) :|| addSilenceToBegin n ts
addSilenceToBegin n (t1 :|| t2) = (addSilenceToBegin n t1) :|| (addSilenceToBegin n t1)

	
addSilenceToTheEnd :: Int -> Track -> Track
addSilenceToTheEnd n  t@(MakeTrack i dp) = 
  let sizedp = lengthDP dp 
  in if (sizedp<n)
     then MakeTrack i (dp :| genSilence (n-sizedp))
	 else t
addSilenceToTheEnd n (t@(MakeTrack i dp) :|| ts) =
 let sizedp = lengthDP dp 
  in if (sizedp<n)
     then ((MakeTrack i (dp :| genSilence (n-sizedp))) :|| addSilenceToTheEnd n ts)
	 else (t :|| addSilenceToTheEnd n ts)
addSilenceToTheEnd n (t1 :|| t2) = (addSilenceToTheEnd n t1) :|| (addSilenceToTheEnd n t1)


----- takes two tracks t1 and t2 and returns a triple, 
-- * First element contains all tracks from t1 that do not have instruments equal
-- to instruments in t2 
-- * Secont element all tracks from t2 that have no matching instruments with tracks
-- from t1 
-- * List of tuples containg matching tracks (t1, t2)

separateTracks :: Track -> Track -> (Maybe Track , Maybe Track,  [(Track,Track)])
separateTracks t@(MakeTrack i dp) t2 = case getTrack i t2 of
	Nothing -> (Just t, Just t2,[])
	Just tr2  -> let (b,mt) = removeTrack i t2 
	             in (Nothing, mt, [(t,tr2)])
separateTracks 	(t@(MakeTrack i dp) :|| ts) t2 =  case getTrack i t2 of
	Nothing -> case separateTracks ts t2 of
  		(Nothing, l1,l2) -> (Just t,l1,l2)
		(Just tr,l1,l2) -> (Just (t:||tr),l1,l2)
	Just tr -> 	case removeTrack i t2 of{
	              (_,Nothing)   -> (Just ts,Nothing,[(t,tr)]);
				  (_, Just nt2) ->  (rt1,rt2,(t,tr):lr)
			                      where (rt1,rt2,lr) = separateTracks ts nt2}
			  --let (rt1,rt2,lr) = separateTracks ts nt2
				    --                in (rt1,rt2,(t,tr):lr)
separateTracks (ta :|| tb) t2 = case separateTracks ta t2 of
--	(Nothing,Nothing,l) -> let (rt1,rt2,lr) = separateTracks tb t2
--	                       in (rt1,rts, l ++ lr)
--	(Nothing, Just r, l) -> case removeListOfTracks l t2	
	(tr1,tr2,list) -> case removeListOfTracks list t2 of
						Nothing -> (joinMaybeTracks tr1 (Just tb), tr2,list)
						Just ntr2 -> let (a,b,c) = separateTracks tb ntr2
						             in (joinMaybeTracks tr1 a, joinMaybeTracks tr2 b, list ++c)

joinMaybeTracks :: Maybe Track -> Maybe Track -> Maybe Track
joinMaybeTracks Nothing a = a
joinMaybeTracks t@(Just tr) Nothing = t
joinMaybeTracks (Just tr1) (Just tr2) = Just (tr1 :|| tr2)
		
removeListOfTracks :: [(Track,Track)] -> Track -> Maybe Track
removeListOfTracks [] t = Just t
removeListOfTracks ((MakeTrack i p,_):xs) t2 = case removeTrack i t2 of	
			(False,_) -> removeListOfTracks xs t2
			(True,Nothing) -> Nothing
			(True,Just t) -> removeListOfTracks xs t



removeTracks :: Instrument -> Track -> Maybe Track
removeTracks i1 t@(MakeTrack i2 dp1)
    | i1 == i2 = Nothing
    | otherwise = Just t
removeTracks i1 (t@(MakeTrack i2 dp1) :|| ts)
    | i1 == i2 = removeTracks i1 ts
    | otherwise = case getTrack i1 ts of
		Nothing -> Just t
		Just tr -> Just (t :|| tr)
removeTracks i1 (t1 :|| t2) = case removeTracks i1 t1 of
				Nothing -> removeTracks i1 t2
				Just tr1 -> case removeTracks i1 t2 of
						Nothing -> Just tr1
						Just tr2 -> Just (tr1 :|| tr2)


getTracks :: Instrument -> Track -> [Track]
getTracks i1 t@(MakeTrack i2 dp1)
    | i1 == i2 = [t]
    | otherwise = []
getTracks i1 (t@(MakeTrack i2 dp1) :|| ts)
    | i1 == i2 = [t]
    | otherwise = getTracks i1 ts
getTracks i1 (t1 :|| t2) = (getTracks i1 t1) ++ (getTracks i1 t2)


																 
getTrack :: Instrument -> Track -> Maybe Track
getTrack i1 t@(MakeTrack i2 dp1)
	| i1 == i2 = Just t
    | otherwise = Nothing
getTrack i1 (t@(MakeTrack i2 dp1) :|| ts)
	| i1 == i2 = Just t
    | otherwise = getTrack i1 ts
getTrack i1 (t1 :|| t2) = case getTrack i1 t1 of	
							Just t -> Just t
							Nothing -> getTrack i1 t2


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


headTr :: Track -> Track
headTr t@(MakeTrack i p)  = t
headTr t@(MakeTrackE i e p)  = t
headTr (t@(MakeTrack i p) :|| p2) = t
headTr (t@(MakeTrackE i e p) :|| p2) = t 
headTr (p1 :|| p2) = headTr p1

tailTr :: Track -> Track
tailTr (MakeTrack i p)  = error "tail of a single Track"
tailTr (MakeTrackE i e p)  = error "tail of a single Track"
tailTr (MakeTrack i p :|| p2) = p2
tailTr (MakeTrackE i e p :|| p2) = p2
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


multTrack :: Int -> Int -> Track -> Track
multTrack size n (MakeTrack i dp) = let sizedp = lengthDP dp 
                                    in  case (sizedp < size) of
										True -> MakeTrack i (n .* (dp :| genSilence (size - sizedp)))
										False -> MakeTrack i (n .* dp)
multTrack size n (MakeTrackE i e dp) = let sizedp = lengthDP dp 
                                    in  case (sizedp < size) of
										True -> MakeTrackE i e (n .* (dp :| genSilence (size - sizedp)))
										False -> MakeTrackE i e (n .* dp)
multTrack size n ((MakeTrack i dp):|| t) = let sizedp = lengthDP dp 
                                           in  case (sizedp < size) of
												True -> (MakeTrack i (n .* (dp :| genSilence (size - sizedp)))) :|| multTrack size n t
												False -> (MakeTrack i (n .* dp)) :|| multTrack size n t
multTrack size n ((MakeTrackE i e dp):|| t) = let sizedp = lengthDP dp 
                                           in  case (sizedp < size) of
												True -> (MakeTrackE i e (n .* (dp :| genSilence (size - sizedp)))) :|| multTrack size n t
												False -> (MakeTrackE i e (n .* dp)) :|| multTrack size n t


genSilence :: Int -> MPattern
genSilence n = takeBeats n infiniteSilence
	where	infiniteSilence = O :| infiniteSilence

-----------------------

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

----------------- composition examples for paper

track1 =  
  MakeTrack "drum_bass_hard"          (X)
  :|| MakeTrack "drum_snare_hard"     (O :| O :| X)
  :|| MakeTrack "drum_cymbal_closed"  (X :| X :| X :| X)


t1  =  
  MakeTrack "drum_bass_hard"          (X :| X)
  :|| MakeTrack "drum_snare_hard"     (O :| O :| X :| O)
  
t2 =  MakeTrack "drum_cymbal_closed"  (X :| X :| X :| X)

t3 = MakeTrack "ambi_choir" X

track2 =  MakeTrack "BassDrum"  (X :| O :| O :| O)
  :|| MakeTrack "AcousticSnare" (O :| O :| X :| O)
  :|| MakeTrack "ClosedHiHat"   (X :| O :| X )
  :|| MakeTrack "Cowbell"       (X)
  

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

lengthDP :: MPattern -> Int
lengthDP O = 1
lengthDP X = 1
lengthDP (X:|p) = 1 + lengthDP p
lengthDP (O:|p) = 1 + lengthDP p
lengthDP (x:|y) = lengthDP x + lengthDP y 


lengthTrack :: Track -> Int
lengthTrack (MakeTrack _ dp) = lengthDP dp
lengthTrack (MakeTrackE _ e dp) = lengthDP dp
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


tailD :: MPattern -> MPattern
tailD (O:|p) = p 
tailD (X:|p) = p
tailD (x:|y) = tailD x :| y

getBeat :: Track -> [Instrument]
getBeat (MakeTrack i O)=[]
getBeat (MakeTrack i X)= [i]
getBeat (MakeTrack i (X:|p))= [i]
getBeat (MakeTrack i (O:|p))= []
getBeat (MakeTrack i (x:| y)) = getBeat (MakeTrack i x)
getBeat (MakeTrack i X :|| t)= i:getBeat t
getBeat (MakeTrack i O :|| t)= getBeat t
getBeat ((MakeTrack i (O:|p)) :|| t)= getBeat t
getBeat ((MakeTrack i (X:|p)) :|| t)= i: getBeat t
getBeat ((MakeTrack i (x:|y)) :|| t)= getBeat (MakeTrack i x) ++  getBeat t
getBeat (t1 :|| t2) = getBeat t1 ++ getBeat t2
--getBeat ((MakeTrack i dp))
removeBeat :: Track -> Maybe Track
removeBeat (MakeTrack i O)= Nothing
removeBeat (MakeTrack i X)= Nothing
removeBeat (MakeTrack i (X:|p))= Just (MakeTrack i p)
removeBeat (MakeTrack i (O:|p))= Just (MakeTrack i p)
removeBeat (MakeTrack i (x:|y))= Just (MakeTrack i (tailD (x:|y)))
removeBeat (MakeTrack i X :|| t)= removeBeat t 
removeBeat (MakeTrack i O :|| t)= removeBeat t 
removeBeat ((MakeTrack i (x:|p)) :|| t)= case removeBeat t of
    Just tf -> Just $ MakeTrack i (tailD (x:|p)) :|| tf
    Nothing -> Just $ MakeTrack i (tailD (x:|p))
removeBeat (t1 :|| t2)= case removeBeat t1 of
    Just tf1 -> case removeBeat t2 of
                 Just tf2 -> Just $ tf1 :|| tf2
                 Nothing -> Just tf1 
    Nothing -> removeBeat t2 

listOfBeats :: Track -> [[Instrument]]
listOfBeats t = case removeBeat t of
					Just tr -> beat : listOfBeats tr
					Nothing -> [beat]
   where beat = getBeat t


--------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------
------------------ THE SONIC PI BACK-END -----------------------------------
---------------------------------------------------------------------------


musicState :: IORef (Maybe Track)
musicState = unsafePerformIO (newIORef Nothing)

genSonicPI :: Float  -> Track -> String
genSonicPI time track = genSonicPI_ time (listOfBeats track)

genSonicPI_ :: Float -> [[Instrument]] -> String
genSonicPI_ time [] = ""
genSonicPI_ time (i:xs) = genNotes i ++ "\tsleep " ++ show time ++ "\n" ++ genSonicPI_ time xs

genNotes :: [Instrument] -> String
genNotes [] = ""
genNotes (i:xs) = "\tsample :" ++ i++ ", rate: 1\n" ++ genNotes xs


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
        
	writeFile "HMusic_temp.rb" $ "live_loop :hmusic do\n" ++ genSonicPI (60/bpm) track ++ "end"
	v <- system $ sonicPiToolPath++"sonic-pi-tool eval-file HMusic_temp.rb"
    	print $ show v

applyToMusic :: (Track -> Track) -> IO ()
applyToMusic ftrack = do
    v <- readIORef musicState
    case v of
        Just t -> do 
                let newTrack = ftrack t
                print $ show newTrack
                writeIORef musicState (Just newTrack)
		writeFile "HMusic_temp.rb" $ "live_loop :hmusic do\n" ++ genSonicPI 0.3 newTrack ++ "end"
		v <- system $ sonicPiToolPath ++ "sonic-pi-tool eval-file HMusic_temp.rb"
    		print $ show v
        Nothing -> error "No running track to be modified"

startMusicServer :: IO ()
startMusicServer = do
   v <- system $ sonicPiToolPath ++ "sonic-pi-tool start-server"
   print $ show v

stopMusicServer :: IO ()
stopMusicServer = do 
   v <- system $ sonicPiToolPath ++ "sonic-pi-tool stop"
   print $ show v

