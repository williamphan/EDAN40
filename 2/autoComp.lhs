> module AutoComp where
> import Haskore hiding (Key)
> import Data.Ratio
> import Data.List

Now we will define a few datatypes which are critical to make the program and our functions work.

> fd d n = n d v
> vol  n = n   v
> v      = [Volume 80]
> cmap f l = chord (map f l)
> lmap f l = line (map f l)
> times  1    m = m
> times n m = m :+: (times (n - 1) m)

> type Key = (PitchClass, Mode)
> type Chord = [Pitch]  
> type ChordProgression = [(Chord, Dur)]
> type Scale = [Int]

> type Scale = [Int]
> ionian, lydian, mixolydian, aeolian, dorian, phrygian :: Quality
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]

> major = [ionian, mixolydian, [], lydian, mixolydian, [], []]
> minor = [[], dorian, phrygian, [], [], aeolian, []]

-----------------------------------------------------------------------------------------

List of all notes with a paired Int // TA BORT I SLUTET

> noteList = zip (cycle [A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs]) [0, 1..]

Function to retrieve a matching Int from the list above

> getInt :: [(PitchClass, Int)] -> PitchClass -> Int
> getInt (x:xs) p
>  | fst x == p = snd x 
>  | otherwise = getInt xs p 

From Int to pitch

> getPitch :: [(PitchClass, Int)] -> Int -> PitchClass
> getPitch (x:xs) n
>  | snd x == n = fst x
>  | otherwise = getPitch xs n

-----------------------------------------------------------------------------------------

Defining the three different types that a bass pattern can have.

> type BassStyle = [(Int, Dur)] 
> basic, calypso, boogie :: BassStyle
> basic = [(0, hn), (4, hn)]
> calypso = [(-1, qn), (0, en), (2, en), (-1, qn), (0, en), (2, en)]
> boogie = [ (0, en), (4, en), (5, en), (4, en), (0, en), (4, en), (5, en), (4, en)]

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass b k c = foldr1 (:+:) $ map (bassLine b k) c 

> bassLine :: BassStyle -> Key -> Music
> bassLine b k
> 	| snd k == hn = foldr1 (:+:) $ pitchDur (bassScale (half b) (keyScale (fst k) ionian)) (half b)
>	| otherwise = foldr1 (:+:) $ pitchDur (bassScale b (keyScale (fst k) ionian)) b

> bassScale :: BassStyle -> [Pitch] -> [Pitch]
> bassScale b ps = map ((!!) ps) (map  fst b)

> pitchDur :: [Pitch] -> BassStyle -> [Music]
> pitchDur (p:ps) (b:bs)
>	| null ps || null bs = [Note p (snd b) v]
>	| (null ps || null bs) && (fst b) == -1 = [Rest (snd b)]
>	| (fst b) == -1 = (Rest (snd b)) : pitchDur ps bs
>	| otherwise =  (Note p (snd b) v) : pitchDur ps bs

> half :: BassStyle -> BassStyle
> half b = fst $ splitAt (length b `div` 2) b


> keyScale :: Pitch -> Scale -> [Pitch]
> keyScale p s = map pitch $ map (+ (absPitch p)) (major !! maybe (-1) id (elemIndex (mod (absPitch p) 12) s))

-------------------------------------------------------------

{-

autoChord :: Key -> ChordProgression -> Music
autoChord k c = 

autoComp :: BassStyle -> Key -> ChordProgression -> Music
autoComp s k c = 

-}