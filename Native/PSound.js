var mySound;

function preload() {
  mySound = loadSound('Electro.wav');
}

var make = function make(localRuntime, mySound) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Http = localRuntime.Native.Http || {};

    if (localRuntime.Native.Http.values) {
        return localRuntime.Native.Http.values;
    }

    var List = Elm.Native.List.make(localRuntime);
    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var Signal = Elm.Native.Signal.make(localRuntime);

    var getRandom = function(Task){
      return function() {
        return Task.asyncFunction(function(callback){
          return callback(Task.succeed(Math.getRandom()));
        });
      };
    };

    var addOne = function(a) {
      return a + 1;
    };

    function playFile(mySound){
      return Task.asyncFunction(function(callback) {
            
        /*var audio = new Audio(String);
        audio.loop = true;
        audio.play();*/

        console.log(mySound.duration());
        mySound.play();

        return callback(Task.succeed(Utils.Tuple0));
      });
    }

    var listOfThings = List.fromArray([1,2,3,4,5]);

    
    return {
      'addOne': addOne,
      'getRandom': getRandom(Task),
      'playFile': playFile(mySound),
      'listOfThings': listOfThings
    };
};

Elm.Native.PSound = {};
Elm.Native.PSound.make = make;