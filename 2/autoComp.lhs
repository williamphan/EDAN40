\begin{document}

\section{Overview}
This file describes how additional functions can be implemented together with Haskore, a library of Haskell modules that work with music.

\begin{code}
module AutoComp where
import Haskore
\end{code}

\section{Datatypes}
Following we have defined several datatypes that are critical in making the functions work.

\begin{code}
type Quality = [Int]
ionian, lydian, mixolydian, aeolian, dorean, phrygian :: Quality
ionian		= [0, 2, 4, 5, 7, 9, 11]
lydian		= [0, 2, 4, 6, 7, 9, 11]
mixolydian	= [0, 2, 4, 5, 7, 9, 10]
aeolian		= [0, 2, 3, 5, 7, 8, 10]
dorean		= [0, 2, 3, 5, 7, 9, 10]
phrygian	= [0, 1, 3, 5, 7, 8, 10]

type Key = (PitchClass, Quality)
--type Pitch = (PitchClass, Octave) 
--type Octave = Int
type Dur = Int
type Chord = [Pitch]
type ChordProgression = [Chord]
{-
testQ :: Quality -> Int
testQ q
 | q == [1, 0]	= 0
 | otherwise	= 1
 -}
\end{code}


\section{autoBass}
This function will generate a bass line in form of a Haskore music object from a bass pattern, chord progression and the key of the song.

\begin{code}
--autoBass :: BassStyle -> Key -> ChordProgression -> Music
--autoBass _ _ [] =
\end{code}

\end{document}