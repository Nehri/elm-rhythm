//Javascript that sends data to elm

var mySound;
var amp;

function preload() {
 mySound = loadSound('Electro.wav');

// initializer message
lmRuntime.ports.ampharos.send(
    { amplitude: 0.0 }
    );
}

function setup() {
  //start playing the song
  mySound.setVolume(0.1);
  mySound.play();

  amp = new p5.Amplitude();
  setInterval(sendAmp, 16);
  
}

//sends the current amplitude
function sendAmp() {

  val = amp.getLevel();
  elmRuntime.ports.ampharos.send(
    { amplitude : val }
    );
  console.log(val);
}