module PSound where

import Time exposing (second, Time)
import Task exposing (Task)
import Native.PSound

--plays a sound file
playFile : String -> Task x ()
playFile = Native.PSound.playFile

-- Native tutorial functions
addOne : Int -> Int
addOne = Native.PSound.addOne

getRandom : Task x Float
getRandom = Native.PSound.getRandom
