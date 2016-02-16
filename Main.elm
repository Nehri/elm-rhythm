module Main where

import Graphics.Element exposing (show)
import Task exposing (Task)
import TaskTutorial exposing (print)
import Time exposing (second, minute, Time)

import PSound exposing (..)
import Graphics.Element exposing (show)

-- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every (second*20)

-- Turn the clock into a signal of tasks
playOn : String -> Signal a -> Signal (Task x ())
playOn str sig =
  Signal.sampleOn sig (Signal.constant (playFile str))

-- Actually perform all those tasks
port runner : Signal (Task x ())
port runner =
  playOn "Electro.wav" (Signal.constant 1)

main = show <| addOne 10
