----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                          Imports                                         --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

module Main where

import Time exposing (Time, fps, timestamp)
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Text exposing (fromString)

import Signal
import Window
import Color
import Keyboard



----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                      Type Defintions                                     --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  Wrapper containing the list of peaks, 
  number of hits, number of misses, 
  the line, bpm, and song start time.
--}
type alias State = (List PeakObject, Int, Int, LineObject, Int, Time)

initState : State
initState = ([], 0, 0, {direction = 0, height = 1, speed = 0}, 0, 0)

{--
  Merged signals relating to user input and peak analysis.
--}
type InputSignal = InitData {peaks : List Float, start : Time, bpm : Int} 
                 | Click (Time,Bool) 
                 | TimeUpdate (Time, Time)

{--
  The per-second analysis data that is shown in the background,
  separate from the peak analysis, received from Javascript.
--}
type alias RealTimeData = 
    { 
      amplitude    : Float,
      bass_energy  : Float,
      low_energy   : Float,
      mid_energy   : Float,
      high_energy  : Float,
      treble_energy: Float
    }

{--
  Peak analysis data sent at the beginning of the song from the Javascript side.
--}
type alias InitialData = 
  { 
    peaks : List Float,
    start : Time,
    bpm   : Int
  }

{--
  Wrapper for the moving line on the page.
--}
type alias LineObject = 
  { 
    direction : Int,
    height    : Float,
    speed     : Float
  }

{--
  Wrapper for each peak found in a song.
--}
type alias PeakObject = 
  { 
    timeDelta : Float,
    clicked   : Bool
  }

----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                           Constants                                      --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

hitImage : String
hitImage = 
    "https://uxtraining.com/assets/UX2-f717a856d969481dceffd400d6cfaf2c.png"

missImage : String
missImage =
    "http://www.clker.com/cliparts/5/9/5/4/12456868161725760927raemi_Cross_Out.svg.med.png"

----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                            Ports                                         --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  Port that accepts real-time amplitude/frequency data from Javascript.
  Named after the Lighthouse Pokemon, Ampharos : 
    http://static.zerochan.net/Ampharos.full.1491150.jpg
--}
port ampharos : Signal RealTimeData

{--
  Port that accepts timing and peak info once at the beginning of runtime.
  Named after the Sheep Pokemon, Flaaffy : 
    http://rs1129.pbsrc.com/albums/m509/2ne1dunsparce/Pokemon/Flaaffy.jpg~c200
--}
port flaaffy : Signal InitialData

----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                     State Update Functions                               --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  Updates the state based on the signal received.
  With a signal of:
    InitData   -> Saves the data into the state to start the game.
    Click      -> Sets relevant peaks to clicked
    TimeUpdate -> Updates the score and deletes past peaks from the 
      front of the list
--}
update : InputSignal -> State -> State
update inputSig (peaks, hits, misses, line, bpm, start) =
  case inputSig of
    InitData data               -> 
      --let speed = 2/1800 in
      let speed = (0.5*(toFloat data.bpm)) / 60000.0 in
      let line' = { line | speed = speed } in
        (toPeakObjects data, 0, 0, line', data.bpm, data.start)
    Click (current,b)           -> 
      ((clickPeaks current start b peaks), hits, misses, line, bpm, start)
    TimeUpdate (current, delta) -> 
      let line' = updateLine delta line in
      let (ps', hits',misses') = updateScore current (peaks, hits, misses, line, bpm, start) in
        (ps', hits', misses', line', bpm, start)

{--
  Turns the TimeDeltas (Floats) received from Javascript into PeakObjects
--}
toPeakObjects : InitialData -> List PeakObject
toPeakObjects data =
    List.map (\time -> {timeDelta = time, clicked = False}) data.peaks

{--
  Checks if the time the key was pressed coincides with any peaks and
  updates them accordingly.
--}
clickPeaks : Time -> Time -> Bool -> List PeakObject -> List PeakObject
clickPeaks current start b peaks =
  case peaks of
    []     -> []
    p::ps  -> 
      let timeDistance = (start + (p.timeDelta * 1000)) - current in
        if timeDistance > -175 && timeDistance < 75 then
          if b then
          {p | clicked = True}::(clickPeaks current start b ps)
          else 
          p::(clickPeaks current start b ps)
        else
          peaks

updateLine : Time -> LineObject -> LineObject
updateLine delta line =
  --moving downwards
  if line.direction == 0 then
    if line.height-(line.speed*delta) < -1 then
      { line | direction = 1, height = ((-1.0)-((line.height-(line.speed*delta))+1))}
    else
      { line | height = line.height - (line.speed * delta) }
  --moving upwards
  else
    if line.height+(line.speed*delta) > 1 then
      { line | direction = 0, height = ((1.0)-((line.height+(line.speed*delta))-1))}
    else
      { line | height = line.height + (line.speed*delta) }

{--
  Adds points to the hit or miss lists according to if past peaks were
  hit or missed before removing those peaks from the list.
--}
updateScore : Time -> State -> (List PeakObject, Int, Int)
updateScore current (peaks, hits, misses, line, bpm, start) =
  case peaks of
      []     -> ([], hits, misses)
      p::ps  -> 
        let timeDistance = (start + (p.timeDelta * 1000)) - current in
          if timeDistance < -300 then
            if p.clicked then
              updateScore current (ps, hits+1, misses, line, bpm, start)
            else
              updateScore current (ps, hits, misses+1, line, bpm, start)
          else
            (peaks, hits, misses)

----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                     Drawing Functions                                    --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  Draws a thick black line across the width of the screen.
--}
linePosition : (Float,Float) -> C.Form
linePosition (w,h) = 
  C.traced { defaultLine | width = 12 } (C.path [(-w,h),(w, h)])

{--
  Draws a coloured dot to represent a peak.
--}
drawCircle : Color.Color -> Float -> C.Form
drawCircle color r = 
  C.filled color (C.circle r)

{--
  Draws an image repesenting a hit or missed peak.
--}
drawImage : String -> Int -> C.Form
drawImage url r =
    url
    |> E.image r r
    |> C.toForm

{--
  Draws 5 transparent circles in the background to represent different frequency
  ranges in the song.
--}
drawBackground : Int -> RealTimeData -> List C.Form
drawBackground w rt =
  [
    ( C.moveX (toFloat (-1*(w//3))) (drawCircle (Color.rgba 0 52 48 0.05) rt.bass_energy) ),
    ( C.moveX (toFloat (-1*(w//6))) (drawCircle (Color.rgba 13 78 73 0.05) rt.low_energy) ),
    ( C.moveX 0.0                   (drawCircle (Color.rgba 35 104 99 0.05) rt.mid_energy) ),
    ( C.moveX (toFloat (w//6))      (drawCircle (Color.rgba 65 131 126 0.05) rt.high_energy) ),
    ( C.moveX (toFloat (w//3))      (drawCircle (Color.rgba 105 157 153 0.05) rt.treble_energy) )
  ]

{--
  Adds the current score to the top right corner of the screen.
--}
drawScore : (Float, Float) -> Int -> Int -> C.Form
drawScore (w,h) hits misses =
  C.move (w/2-100,h/2-100) (C.text 
    (Text.typeface ["avant garde", "arial"] (Text.height 30 (Text.color (Color.rgba 138 0 94 0.5) 
                      (fromString 
                        ((toString hits)++" / "++(toString (hits+misses))))))))

{--
  Draws a dot ahead of time in the position the line will be in at the time 
  the peak happens in the music.
--}
drawPeak : (Int, Int) -> PeakObject -> LineObject -> Time -> Float -> C.Form
drawPeak (w,h) peak line timeDistance r =
  let futurePos = updateLine timeDistance line in
  let h2 = futurePos.height in      
  let w' = (w-100) in
  let w2 =
  let mod =  ((round (peak.timeDelta * 100)) % (2*w')) in
    if mod < w' then (w'//(-2)) + mod
    else (w'//2) - (mod%w')
  in
    if peak.clicked then
      (C.move (toFloat w2, h2*(toFloat (h//2))) (drawImage hitImage (round (2*r))))
    else if timeDistance < -175 then
      (C.move (toFloat w2, h2*(toFloat (h//2))) (drawImage missImage (round (2*r))))
    else if futurePos.direction == 0 then
      (C.move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 95 86 255 0.8) r))
    else
      (C.move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 124 255 153 0.8) r))

{--
  Cycles through the peaks in the state and draws any that will be happening soon.
--}
drawPeaks : (Int, Int) -> Time -> State -> List C.Form
drawPeaks (w,h) current (peaks, hits, misses, line, bpm, start) =
    case peaks of
      []     -> []
      p::ps  -> 
          let timeDistance = (start + (p.timeDelta * 1000)) - current in
            --Beats that are passed
            if timeDistance < -300 then
              drawPeaks (w,h) current (ps, hits, misses, line, bpm, start)
            --Beats that are too far away
            else if timeDistance > 700 then
              []
            else
              let r =
                if timeDistance < -200 then 10
                else if timeDistance < -100 then 25
                else if timeDistance < -50 then 35
                else if timeDistance < 0 then 45
                else if timeDistance < 300 then 50
                else if timeDistance < 350 then 45
                else if timeDistance < 400 then 40
                else if timeDistance < 450 then 35
                else if timeDistance < 500 then 30
                else if timeDistance < 550 then 25
                else if timeDistance < 600 then 22
                else if timeDistance < 650 then 20
                else 10 in
                  (drawPeak (w,h) p line timeDistance r)::(drawPeaks (w,h) current (ps, hits, misses, line, bpm, start))

{--
  Adds the score, line, dots, and background circles together for the page.
--}
view : (Int, Int) -> RealTimeData ->  (Time, State) -> E.Element
view (w,h) rt (t, (peaks, hits, misses, line, bpm, start)) =
  let (w',h') = (w, h-150) in
    C.collage w (h-100) ((linePosition (toFloat w,line.height*(toFloat (h'//2))))::
      (drawScore (toFloat w,toFloat h) hits misses)::
      (List.append (drawPeaks (w',h') t (peaks, hits, misses, line, bpm, start)) (drawBackground w rt)))

----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                            Main                                          --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  Merges signals from various inputs to update state and pass both real-time and past-dependent
  data to be output to the screen.
--}
main : Signal E.Element
main = 
  [(Signal.map InitData flaaffy),(Signal.map Click (timestamp Keyboard.space)),(Signal.map TimeUpdate (timestamp (fps 30)))]
    |> Signal.mergeMany
    |> Signal.foldp update initState
    |> timestamp
    |> Signal.map3 view Window.dimensions ampharos


----------------------------------------------------------------------------------------------
--                                                                                          --
--                                                                                          --
--                                    Deprecated Functions                                  --             
--                                                                                          --
--                                                                                          --
----------------------------------------------------------------------------------------------

{--
  An initial value for the data received from port Ampharos.
--}
silentMusic : RealTimeData
silentMusic =
    { amplitude = 0.0,
      bass_energy = 0.0,
      low_energy = 0.0,
      mid_energy = 0.0,
      high_energy = 0.0,
      treble_energy = 0.0
    }

{--
  Decodes the amplitude from JSON received from port Ampharos.
--}
{--
floatToObject : Decoder RealTimeData
floatToObject = 
    let soundDecoder = Decode.object1 RealTimeData ("amplitude" := Decode.float) in
    Decode.customDecoder soundDecoder
        (\sound ->
            Ok { silentMusic | amplitude = sound.amplitude
            }
        )
--}

{--
  Encodes an amplitude value into the JSON sent to Javascript.
--}
{--
objectToValue : RealTimeData -> Encode.Value
objectToValue sound = 
    Encode.object <|
        [ ("amplitude", Encode.float sound.amplitude)]
--}
