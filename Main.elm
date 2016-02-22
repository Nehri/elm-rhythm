module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (millisecond, second, minute, Time)

import Graphics.Element exposing (show, flow, Element)
import Graphics.Collage exposing (Form, collage, toForm, filled, circle, move)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

import Signal
import Window
import Color

type alias MusicObject = 
    { amplitude  : Float,
      low_energy : Float,
      mid_energy : Float,
      high_energy: Float
    }

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every millisecond

-- Play the given sound file on the given signal
--playOn : String -> Signal a -> Signal (Task x ())
--playOn str sig =
  --Signal.sampleOn sig (Signal.constant)

-- Actually perform all those tasks
--port runner : Signal (Task x ())
--port runner =
  --playOn "Electro.wav" (Signal.constant 1) 

drawCircle : Color.Color -> Float -> Form
drawCircle color r = 
  filled Color.black (circle r)

showShape : Color.Color -> Float -> Element
showShape color r = 
  collage 2500 1000 [(move (0,0) (drawCircle color r))]

--main = Signal.map show (Signal.sampleOn clock (Signal.map (\a -> a.amplitude) ampharos))

silentMusic : MusicObject
silentMusic =
    { amplitude   = 0.0, 
      low_energy  = 0.0,
      mid_energy  = 0.0,
      high_energy = 0.0
     }

--objectToValue : MusicObject -> Encode.Value
--objectToValue sound = 
    --Encode.object <|
        --[ ("amplitude", Encode.float sound.amplitude)]

--floatToObject : Decoder MusicObject
--floatToObject = 
    --let soundDecoder = Decode.object1 MusicObject ("amplitude" := Decode.float) in
    --Decode.customDecoder soundDecoder
        --(\sound ->
        --    Ok { silentMusic | amplitude = sound.amplitude
        --    }
        --)

--Port that accepts current sound info from Javascript
port ampharos : Signal MusicObject

main = Signal.sampleOn (Signal.map (\a -> a.amplitude) ampharos) (Signal.map (\a -> showShape Color.black (a.amplitude * 700)) ampharos)
