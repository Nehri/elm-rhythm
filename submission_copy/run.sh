#!/bin/bash

python importSound.py
cd src
elm-make Main.elm --output out.js
firefox ./out.html