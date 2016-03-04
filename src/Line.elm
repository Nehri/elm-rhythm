module Line where

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

type alias State = LineObject

initState = {direction = 0, height = 0}

type alias LineObject = 
  { direction: Int,
    height: Float
  }

offset = round ((60/70)/1000)

updateDirection : Int -> State -> State
updateDirection h line = 
  --moving downwards
  if line.direction == 0 then
    if line.height-offset < -h then
      { direction = 1, height = -h }
    else
      { line | height = line.height - offset }
  --moving upwards
  else
    if line.height+offset > h then
      { direction = 0, height = h }
    else
      { line | height = line.height + offset }

linePosition : (Int,Int) -> Form
linePosition (w,h) = 
  traced defaultLine (path [(toFloat -w, toFloat h),(toFloat w, toFloat h)])

  -- A signal that updates to the current time every second
clock : Signal Time
clock =
  Time.every millisecond

 view : (Int, Int) -> State -> Element
view (w,h) st =
  collage w h [
    (linePosition (w,st.height))
    ]

main : Signal Element
main = Signal.map2 view Window.dimensions (Signal.foldp updateDirection initState clock)