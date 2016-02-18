module Music where

import Time exposing (second, Time)
import Task exposing (Task)
import Native.Music

--plays a sound file
playFile : String -> Task x ()
playFile = Native.Music.playFile

-- Native tutorial functions
addOne : Int -> Int
addOne = Native.Music.addOne

getRandom : Task x Float
getRandom = Native.Music.getRandom

--getMasterVolume : Float
--getMasterVolume = Native.Music.getMasterVolume