module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (millisecond, second, minute, Time, fps, timestamp)

import Graphics.Element exposing (show, flow, Element, image)
import Graphics.Collage exposing (Form, collage, toForm, filled, circle, move, moveX, moveY, traced, defaultLine, path, ngon, text)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

import Signal
import Window
import Color
import Keyboard

import Text exposing (fromString)

{--
  Wrapper containing the list of peaks, 
  number of hits, number of misses, 
  the line, bpm, and song start time.
--}
type alias State = (List PeakObject, Int, Int, LineObject, Int, Time)

initState : State
initState = ([], 0, 0, {direction = 0, height = 1, speed = 0}, 0, 0)

type InputSignal = InitData {peaks : List Float, start : Time, bpm : Int} | Click (Time,Bool) | TimeUpdate (Time, Time)

type alias RealTimeData = 
    { amplitude    : Float,
      bass_energy  : Float,
      low_energy   : Float,
      mid_energy   : Float,
      high_energy  : Float,
      treble_energy: Float
    }

type alias InitialData = 
  { peaks : List Float,
    start : Time,
    bpm   : Int
  }

type alias LineObject = 
  { direction : Int,
    height    : Float,
    speed     : Float,
  } 

type alias PeakObject = 
  { timeDelta : Float,
    clicked   : Bool
  }

hitImage : String
hitImage = 
    "https://uxtraining.com/assets/UX2-f717a856d969481dceffd400d6cfaf2c.png"

missImage : String
missImage =
    "http://www.clker.com/cliparts/5/9/5/4/12456868161725760927raemi_Cross_Out.svg.med.png"


update : InputSignal -> State -> State
update inputSig (peaks, hits, misses, line, bpm, start) =
  case inputSig of
    InitData data               -> 
      (toPeakObjects data, 0, 0, line, data.bpm, data.start)
    Click (current,b)           -> 
      ((clickPeaks current start b peaks), hits, misses, line, bpm, start)
    TimeUpdate (current, delta) -> 
      let line' = updateLine bpm delta line in
      let (ps', hits',misses') = updateScore current (peaks, hits, misses, line, bpm, start) in
        (ps', hits', misses', line', bpm, start)

toPeakObjects : InitialData -> List PeakObject
toPeakObjects data =
    List.map (\time -> {timeDelta = time, clicked = False}) data.peaks

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

updateLine : Int -> Time -> LineObject -> LineObject
updateLine bpm delta line =
    --let speed = 2/1800
    let speed = (2.0*(toFloat bpm)) / 60000.0 in
      --moving downwards
      if line.direction == 0 then
        if line.height-(speed*delta) < -1 then
          { direction = 1, height = ((-1.0)-((line.height-(speed*delta))+1))}
        else
          { line | height = line.height - (speed * delta) }
      --moving upwards
      else
        if line.height+(speed*delta) > 1 then
          { direction = 0, height = ((1.0)-((line.height+(speed*delta))-1))}
        else
          { line | height = line.height + (speed * delta) }

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

linePosition : (Float,Float) -> Form
linePosition (w,h) = 
  traced { defaultLine | width = 12 } (path [(-w,h),(w, h)])

drawCircle : Color.Color -> Float -> Form
drawCircle color r = 
  filled color (circle r)

drawImage : String -> Int -> Form
drawImage url r =
    url
    |> image r r
    |> toForm

drawBackground : Int -> RealTimeData -> List Form
drawBackground w rt =
  [
    ( moveX (toFloat (-1*(w//3))) (drawCircle (Color.rgba 0 52 48 0.05) rt.bass_energy) ),
    ( moveX (toFloat (-1*(w//6))) (drawCircle (Color.rgba 13 78 73 0.05) rt.low_energy) ),
    ( moveX 0.0                   (drawCircle (Color.rgba 35 104 99 0.05) rt.mid_energy) ),
    ( moveX (toFloat (w//6))      (drawCircle (Color.rgba 65 131 126 0.05) rt.high_energy) ),
    ( moveX (toFloat (w//3))      (drawCircle (Color.rgba 105 157 153 0.05) rt.treble_energy) )
  ]

drawScore : (Float, Float) -> Int -> Int -> Form
drawScore (w,h) hits misses =
  move (w/2-100,h/2-100) (text 
    (Text.typeface ["avant garde", "arial"] (Text.height 30 (Text.color (Color.rgba 138 0 94 0.5) 
                      (fromString 
                        ((toString hits)++" / "++(toString (hits+misses))))))))

drawPeak : (Int, Int) -> PeakObject -> LineObject -> Time -> Float -> Form
drawPeak (w,h) peak line timeDistance r =
  let futurePos = updateLine bpm timeDistance line in
  let h2 = futurePos.height in      
  let w' = (w-100) in
  let w2 =
  let mod =  ((round (peak.timeDelta * 100)) % (2*w')) in
    if mod < w' then (w'//(-2)) + mod
    else (w'//2) - (mod%w')
  in
    if peak.clicked then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawImage hitImage (round (2*r))))
    else if timeDistance < -175 then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawImage missImage (round (2*r))))
    else if futurePos.direction == 0 then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 95 86 255 0.8) r))
    else
      (move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 124 255 153 0.8) r))

drawPeaks : (Int, Int) -> Time -> State -> List Form
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

view : (Int, Int) -> RealTimeData ->  (Time, State) -> Element
view (w,h) rt (t, (peaks, hits, misses, line, bpm, start)) =
  let (w',h') = (w, h-150) in
    collage w (h-100) ((linePosition (toFloat w,line.height*(toFloat (h'//2))))::
      (drawScore (toFloat w,toFloat h) hits misses)::
      (List.append (drawPeaks (w',h') t (peaks, hits, misses, line, bpm, start)) (drawBackground w rt)))

silentMusic : RealTimeData
silentMusic =
    { amplitude = 0.0,
      bass_energy = 0.0,
      low_energy = 0.0,
      mid_energy = 0.0,
      high_energy = 0.0,
      treble_energy = 0.0
    }

--floatToObject : Decoder RealTimeData
--floatToObject = 
    --let soundDecoder = Decode.object1 RealTimeData ("amplitude" := Decode.float) in
    --Decode.customDecoder soundDecoder
        --(\sound ->
        --    Ok { silentMusic | amplitude = sound.amplitude
        --    }
        --)

--objectToValue : RealTimeData -> Encode.Value
--objectToValue sound = 
    --Encode.object <|
        --[ ("amplitude", Encode.float sound.amplitude)]

--Port that accepts real-time amplitude/frequency data from Javascript
port ampharos : Signal RealTimeData

--Port that accepts time and peak info once at the beginning of runtime
port flaaffy : Signal InitialData

main : Signal Element
main = 
  [(Signal.map InitData flaaffy),(Signal.map Click (timestamp Keyboard.space)),(Signal.map TimeUpdate (timestamp (fps 30)))]
    |> Signal.mergeMany
    |> Signal.foldp update initState
    |> timestamp
    |> Signal.map3 view Window.dimensions ampharos