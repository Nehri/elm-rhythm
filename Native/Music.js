var make = function make(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Http = localRuntime.Native.Http || {};

    if (localRuntime.Native.Http.values) {
        return localRuntime.Native.Http.values;
    }

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

    function playFile(String){
      return Task.asyncFunction(function(callback) {
            
        //var audio = new Audio(String);
        var audio = new Audio(String);
        audio.play();

        return callback(Task.succeed(Utils.Tuple0));
      });
    }

    return {
    	'addOne': addOne,
        'getRandom': getRandom(Task),
        'playFile': playFile
    };
};

Elm.Native.Music = {};
Elm.Native.Music.make = make;