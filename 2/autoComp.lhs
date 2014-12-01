
\begin{code}
module AutoComp where
import Haskore hiding (Key)

-- från Haskore
-- line: foldr1 (:+:)
-- chord: foldr1 (:=:)

type Quality = [Int]
ionian, lydian, mixolydian, aeolian, dorian, phrygian :: Quality
ionian	= [0, 2, 4, 5, 7, 9, 11]
lydian	= [0, 2, 4, 6, 7, 9, 11]
mixolydian = [0, 2, 4, 5, 7, 9, 10]
aeolian = [0, 2, 3, 5, 7, 8, 10]
dorian	= [0, 2, 3, 5, 7, 9, 10]
phrygian = [0, 1, 3, 5, 7, 8, 10]

type BassStyle = [(Int, Dur)] -- vad är Int?
basic, calypso, boogie :: BassStyle
basic = [(0, hn), (4, hn)]
calypso = [	(-1, qn), (0, en), (2, en), 
 			(-1, qn), (0, en), (2, en)]
boogie = [ 	(0, en), (4, en), 
 			(5, en), (4, en), 
 			(0, en), (4, en), 
 			(5, en), (4, en)]


type Key = (PitchClass, Quality)
--type Pitch = (PitchClass, Octave) 
--type Octave = Int
--type Dur = Int
type Chord = [Int]  -- Int ska vara många noter
type ChordProgression = [(Chord, Dur)]

--autoBass :: BassStyle -> Key -> ChordProgression -> Music
--autoBass b k c = line $ map asd

--bassLine :: Key -> Style -> ChordProgression -> [(Note, Dur)]

--bassLine _ _ [] = []



\end{code}
