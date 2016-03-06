module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (millisecond, second, minute, Time, fps, timestamp)

import Graphics.Element exposing (show, flow, Element, image)
import Graphics.Collage exposing (Form, collage, toForm, filled, circle, move, moveX, moveY, traced, defaultLine, path, ngon)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

import Signal
import Window
import Color
import Keyboard

type alias State = LineObject

initState : State
initState = {direction = 0, height = 1}

type InputSignal = InitData {peaks : List Float, start : Time, bpm : Float} | TimeDelta (Time,Bool)

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
    bpm   : Float
  }

type alias LineObject = 
  { direction : Int,
    height    : Float
  } 

type alias PeakObject = 
  { songStart : Time,
    timeDelta : Float,
    clicked   : Bool
  }

offset : Float
offset = 2/1800

hitImage : String
hitImage = 
    "https://uxtraining.com/assets/UX2-f717a856d969481dceffd400d6cfaf2c.png"

missImage : String
missImage =
    "http://www.clker.com/cliparts/5/9/5/4/12456868161725760927raemi_Cross_Out.svg.med.png"

update : Time -> State -> State
update t line = 
    --moving downwards
    if line.direction == 0 then
      if line.height-(offset*t) < -1 then
        { direction = 1, height = ((-1.0)-((line.height-(offset*t))+1))}
      else
        { line | height = line.height - (offset * t) }
    --moving upwards
    else
      if line.height+(offset*t) > 1 then
        { direction = 0, height = ((1.0)-((line.height+(offset*t))-1))}
      else
        { line | height = line.height + (offset * t) }

toPeakObjects : InitialData -> List PeakObject
toPeakObjects data =
  let start = data.start in
    List.map (\time -> {songStart = start, timeDelta = time, clicked = False}) data.peaks

updatePeaks : InputSignal -> (List PeakObject, Int, Int) -> (List PeakObject, Int, Int)
updatePeaks inputSig (peaks, hits, misses) =
  case inputSig of
    InitData data -> (toPeakObjects data, 0, 0)
    TimeDelta (curTime,b) ->
      case peaks of
        []     -> ([], hits, misses)
        p::ps -> 
          let timeDistance = (p.songStart + (p.timeDelta * 1000)) - curTime in
            if timeDistance < -300 then
                if p.clicked then
                  updatePeaks inputSig (ps, hits+1, misses)
                else
                  updatePeaks inputSig (ps, hits, misses+1)
            else if timeDistance > -175 && timeDistance < 75 then
                if b then
                  let (ps', h, m) = updatePeaks inputSig (ps, hits, misses) in
                    ({p | clicked = True}::ps', hits, misses) 
                else
                  (peaks, hits, misses)
            --Beats that are too far away
            else
                (peaks, hits, misses)

linePosition : (Float,Float) -> Form
linePosition (w,h) = 
  traced { defaultLine | width = 12 } (path [(-w,h),(w, h)])

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every millisecond

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

drawPeak : (Int, Int) -> Time -> PeakObject -> Time -> State -> Float -> Form
drawPeak (w,h) curTime peak timeDistance line r =
  let futurePos = update timeDistance line in
  let h2 = futurePos.height in
  let w2 =
  let mod =  ((round (peak.timeDelta * 100)) % (2*w)) in
    if mod < w then (w//(-2)) + mod
    else (w//2) - (mod%w)
  in
    if peak.clicked then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawImage hitImage (round (2*r))))
    else if timeDistance < -175 then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawImage missImage (round (2*r))))
    else if futurePos.direction == 0 then
      (move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 95 86 255 0.8) r))
    else
      (move (toFloat w2, h2*(toFloat (h//2))) (drawCircle (Color.rgba 124 255 153 0.8) r))

drawPeaks : (Int, Int) -> Time -> List PeakObject -> State -> List Form
drawPeaks (w,h) curTime p line =
    case p of
      []     -> []
      p'::ps -> 
        --Skip beats that have already been clicked
        --if p'.clicked == True then
          --drawPeaks (w,h) curTime ps line
        --else
          let timeDistance = (p'.songStart + (p'.timeDelta * 1000)) - curTime in
            --Beats that are passed
            if timeDistance < -300 then
              drawPeaks (w,h) curTime ps line
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
                  (drawPeak (w,h) curTime p' timeDistance line r)::(drawPeaks (w,h) curTime ps line)

view : (Int, Int) -> RealTimeData -> (List PeakObject, Int, Int) -> (Time, State) -> Element
view (w,h) rt (peaks, hits, misses) (t, line) =
  let (w',h') = (w, h-150) in
    collage w (h-100) ((linePosition (toFloat w,line.height*(toFloat (h'//2))))::
      (List.append (drawPeaks (w',h') t peaks line) (drawBackground w rt)))

--objectToValue : RealTimeData -> Encode.Value
--objectToValue sound = 
    --Encode.object <|
        --[ ("amplitude", Encode.float sound.amplitude)]

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

--Port that accepts real-time amplitude/frequency data from Javascript
port ampharos : Signal RealTimeData

--Port that accepts time and peak info once at the beginning of runtime
port flaaffy : Signal InitialData

main : Signal Element
main = Signal.map4 view Window.dimensions ampharos 
      (Signal.foldp updatePeaks ([], 0, 0) (Signal.merge (Signal.map InitData flaaffy) (Signal.map TimeDelta (timestamp Keyboard.space)))) 
      (timestamp (Signal.foldp update initState (fps 30)))
