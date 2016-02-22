//Javascript that sends data to elm
var elmRuntime = Elm.fullscreen(Elm.Main, {"ampharos" : {"amplitude" : 1.0}});

var mySound;
var amp;



function preload() {
 mySound = loadSound('Electro.wav');
 console.log("loaded sound");
}

function setup() {
  //start playing the song
  mySound.setVolume(0.5);
  mySound.play();
  
  console.log("played sound");
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
// initializer message
elmRuntime.ports.ampharos.send(
    { amplitude: 0.0 }
    );

