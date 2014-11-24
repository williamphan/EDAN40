
module Twinkle where
import Haskore

-- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

-- Main Voice:
v1 = v1a :+: v1b :+: v1c :+: v1d
v1a = lmap (fd qn) [c 4, c 4, g 4, g 4, a 4, a 4]
v1b = lmap (fd hn) [g 5]
v1c = lmap (fd qn) [f 4, f 4, e 4, e 4, d 4, d 4]
v1d = lmap (fd hn) [c 5]

v2 = v2a :+: v2b :+: v2a :+: v2b
v2a = lmap (fd qn) [g 4, g 4, f 4, f 4, e 4, e 4]
v2b = lmap (fd hn) [d 5]
mainVoice = v1 :+: v2 :+: v1

twinkle = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))
