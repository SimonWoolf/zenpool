module Config exposing (WaveShape(..), gapSize, numAdditionalWaves, pixelSize, pointScaleFactor, ripplePropagationSpeed, rippleWidth, tickLengthMs, waveFadeFactor, waveShape)

tickLengthMs : Float
tickLengthMs = 50

pixelSize = 40

gapSize = 5

rippleWidth = 2

ripplePropagationSpeed = 0.5

numAdditionalWaves = 2

waveFadeFactor = 2

type WaveShape
    = Circle
    | PointedStar Int

waveShape : WaveShape
waveShape = Circle

pointScaleFactor = 1 / 2
