module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (millisecond, second, minute, Time)

import Graphics.Element exposing (show, flow, Element)
import Graphics.Collage exposing (Form, collage, toForm, filled, circle, moveX, moveY, traced, defaultLine, path)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

import Signal
import Window
import Color

type alias State = (InitialData, RealTimeData, LineObject)

initState = ({peaks = [], start = 0}, 
             silentMusic, {direction = 0, height = 1})

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
    start : Time
  }

type alias LineObject = 
  { direction : Int,
    height    : Float
  } 

offset = round ((60/70)/1000)

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every millisecond

drawCircle : Color.Color -> Float -> Form
drawCircle color r = 
  filled color (circle r)

view : (Int, Int) -> RealTimeData -> Element
view (w,h) obj =
  collage w h [
    ( moveX (toFloat (-1*(w//3))) (drawCircle (Color.rgb 0 52 48) obj.bass_energy) ),
    ( moveX (toFloat (-1*(w//6))) (drawCircle (Color.rgb 13 78 73)   obj.low_energy) ),
    ( moveX 0.0                   (drawCircle (Color.rgb 35 104 99)  obj.mid_energy) ),
    ( moveX (toFloat (w//6))      (drawCircle (Color.rgb 65 131 126) obj.high_energy) ),
    ( moveX (toFloat (w//3))      (drawCircle (Color.rgb 105 157 153)    obj.treble_energy) )
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

--Port that accepts time and peak data once at the beginning of runtime
port flaaffy : Signal InitialData

main : Signal Element
main = Signal.map2 view Window.dimensions ampharos