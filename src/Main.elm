module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (millisecond, second, minute, Time, fps)

import Graphics.Element exposing (show, flow, Element)
import Graphics.Collage exposing (Form, collage, toForm, filled, circle, moveX, moveY, traced, defaultLine, path)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

import Signal
import Window
import Color

type alias State = (InitialData, LineObject)

initState : State
initState = ({peaks = [], start = 0, bpm = 0}, 
                  {direction = 0, height = 1})

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

offset : Float
offset = 2/870

update : Time -> State -> State
update t (data,line) = 
    --moving downwards
    if line.direction == 0 then
      if line.height-(offset*t) < -1 then
        (data, { direction = 1, height = ((-1.0)-((line.height-(offset*t))+1))})
      else
        (data, { line | height = line.height - (offset * t) })
    --moving upwards
    else
      if line.height+(offset*t) > 1 then
        (data, { direction = 0, height = ((1.0)-((line.height+(offset*t))-1))})
      else
        (data, { line | height = line.height + (offset * t) })

linePosition : (Float,Float) -> Form
linePosition (w,h) = 
  traced defaultLine (path [(-w,h),(w, h)])

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every millisecond

drawCircle : Color.Color -> Float -> Form
drawCircle color r = 
  filled color (circle r)

view : (Int, Int) -> RealTimeData -> State -> Element
view (w,h) obj (data,line) =
    collage w h [
      (linePosition (toFloat w,line.height*(toFloat (h//2)))),
      ( moveX (toFloat (-1*(w//3))) (drawCircle (Color.rgba 0 52 48 0.05) obj.bass_energy) ),
      ( moveX (toFloat (-1*(w//6))) (drawCircle (Color.rgba 13 78 73 0.05)   obj.low_energy) ),
      ( moveX 0.0                   (drawCircle (Color.rgba 35 104 99 0.05)  obj.mid_energy) ),
      ( moveX (toFloat (w//6))      (drawCircle (Color.rgba 65 131 126 0.05) obj.high_energy) ),
      ( moveX (toFloat (w//3))      (drawCircle (Color.rgba 105 157 153 0.05)    obj.treble_energy) )
      ]

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

--addPeak : RealTimeData -> State -> State
--addPeak peak (t,m,l) =
  --(t,peak::m,l)

main : Signal Element
main = Signal.map3 view Window.dimensions ampharos (Signal.foldp update initState (fps 30))
