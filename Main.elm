module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import Time exposing (second, minute, Time)

import PSound exposing (..)
import Graphics.Element exposing (show)

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every (second*20)

-- Play the given sound file on the given signal
playOn : String -> Signal a -> Signal (Task x ())
playOn str sig =
  Signal.sampleOn sig (Signal.constant (playFile))

-- Actually perform all those tasks
port runner : Signal (Task x ())
port runner =
  playOn "Electro.wav" (Signal.constant 1)

main = show (List.map ((+) 10) listOfThings)