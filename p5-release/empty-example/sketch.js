// Learning Processing
// Daniel Shiffman
// http://www.learningprocessing.com

// Example 1-1: stroke and fill

var mySound;

function preload() {
	mySound = loadSound('Electro.wav');
}
function setup() {
  	mySound.setVolume(0.5);
  	mySound.play();
}

/*
function mousePressed() {
  // Using the third-party library to call play() on the buzz object
  mySound.play();
}
*/