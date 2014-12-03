> module AutoComp where
> import Haskore hiding (Key)

-- från Haskore
-- line: foldr1 (:+:)
-- chord: foldr1 (:=:)

--type Pitch = (PitchClass, Octave) 
--type Octave = Int
--type Dur = Int

Now we will define a few datatypes which are critical to make the program and our functions work.

> type Key = (Pitch, Quality)
> type Chord = [Int]  
> type ChordProgression = [(Chord, Dur)]
> type Quality = [Int]
> ionian, lydian, mixolydian, aeolian, dorian, phrygian :: Quality
> ionian = [0, 2, 4, 5, 7, 9, 11]
> lydian = [0, 2, 4, 6, 7, 9, 11]
> mixolydian = [0, 2, 4, 5, 7, 9, 10]
> aeolian = [0, 2, 3, 5, 7, 8, 10]
> dorian = [0, 2, 3, 5, 7, 9, 10]
> phrygian = [0, 1, 3, 5, 7, 8, 10]


List of all notes with a paired Int

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


Defining the three different types that a bass pattern can have.

> type BassStyle = [(Int, Dur)] 
> basic, calypso, boogie :: BassStyle
> basic = [(0, hn), (4, hn)]
> calypso = [(-1, qn), (0, en), (2, en), (-1, qn), (0, en), (2, en)]
> boogie = [ (0, en), (4, en), (5, en), (4, en), (0, en), (4, en), (5, en), (4, en)]


> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass b k c = line $ map (bassLine b k) c


Help function to generate bass 

> bassLine :: BassStyle -> Key -> Music
> -- bassLine b k
