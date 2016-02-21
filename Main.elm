module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (second, minute, Time)

import Graphics.Element exposing (show)

import Json.Encode as Encode
import Json.Decode as Decode exposing ((:=), Decoder)

type alias MusicObject = 
    { amplitude : Float
    }

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every (second*20)

-- Play the given sound file on the given signal
--playOn : String -> Signal a -> Signal (Task x ())
--playOn str sig =
  --Signal.sampleOn sig (Signal.constant)

-- Actually perform all those tasks
--port runner : Signal (Task x ())
--port runner =
  --playOn "Electro.wav" (Signal.constant 1)

main = show (List.map ((+) 10) listOfThings)

silentMusic : MusicObject
silentMusic =
    { amplitude = 0.0 }

objectToValue : MusicObject -> Encode.Value
objectToValue sound = 
    Encode.object <|
        [ ("amplitude", Encode.float sound.amplitude)]

floatToObject : Decoder MusicObject
floatToObject = 
    let soundDecoder = Decode.object1 MusicObject ("amplitude" := Decode.float) in
    Decode.customDecoder soundDecoder
        (\sound ->
            Ok { silentMusic | amplitude = sound.amplitude
            }
        )

--plays a sound file
playFile : Task x ()
playFile = Native.PSound.playFile

-- Native tutorial functions
addOne : Int -> Int
addOne = Native.PSound.addOne

getRandom : Task x Float
getRandom = Native.PSound.getRandom

--see if I can figure out how lists work
listOfThings : List a
listOfThings = Native.PSound.listOfThings

--Port that accepts current sound info from Javascript
port ampharos : Signal MusicObject