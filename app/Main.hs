module Main where

import qualified Data.ByteString.Lazy as A
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List
import Data.Fixed

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output"

duration :: Seconds
duration = 2

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 24000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0,0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]


modulator :: (Num a, Floating t) => (t -> a) -> t -> a -> t -> a
modulator baseform freq amp x = amp*baseform(freq*x)

sin2 :: Floating a => a -> a
sin2 x = sin(2*pi*x)

sawtooth :: Real a => a -> a
sawtooth x = 2 * (mod' x 1) - 1

outFunction :: Samples -> Samples
outFunction = modulator sin2 440 1

outputSamples :: [Samples]
outputSamples = map ((* volume) . outFunction . (/ sampleRate)) [0 .. sampleRate * duration]

save :: FilePath -> IO ()
save filePath = A.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE outputSamples

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffmpeg -y -f f32le -ar %f -ac 1 -i %s %s.wav" sampleRate outputFilePath outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = play