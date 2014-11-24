\section{Auto Comp}

\begin{code}
module AutoComp where
import Haskore
\end{code}

Types

\begin{code}
type Pitch = (PitchClass, Octave)
type Octave = Int
type Dur = Int
type Key = (PitchClass, Quality)
type Chord = -- (PitchClass, ???)
type ChordProgression = [Chord]
\end{code}

Scales

\begin{code}
Ionian = [0, 2, 4, 5, 7, 9, 11]
Lydian = [0, 2, 4, 6, 7, 9, 11]
Mixolydian = [0, 2, 4, 5, 7, 9, 10]
Aeolian = [0, 2, 3, 5, 7, 8, 10]
Dorian = [0, 2, 3, 5, 7, 9, 10]
Phrygian = [0, 1, 3, 5, 7, 8, 10]
\end{code}

\section{Function that will automatically generate a bass line.}
\begin{code}
autoBass :: BassStyle -> Key -> ChordProgression -> Music  -- [Chord]?
\end{code}
