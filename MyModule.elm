module MyModule where

import Time exposing (second, Time)
import Task exposing (Task)
import Native.MyModule 

-- this will be our function which returns a number plus one
addOne : Int -> Int
addOne = Native.MyModule.addOne

-- this will be our function which returns a random number
getRandom : Task x Float
getRandom = Native.MyModule.getRandom