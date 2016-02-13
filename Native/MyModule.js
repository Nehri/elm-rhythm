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

var make = function make(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Http = localRuntime.Native.Http || {};

    if (localRuntime.Native.Http.values) {
        return localRuntime.Native.Http.values;
    }

    var Task = Elm.Native.Task.make(localRuntime);
    var Utils = Elm.Native.Utils.make(localRuntime);
    var Signal = Elm.Native.Signal.make(localRuntime);
    var Tuple0 = Utils['Tuple0'];
    var Tuple2 = Utils['Tuple2'];

    return {
    	'addOne': addOne,
        'getRandom': getRandom(Task)
    };
};

Elm.Native.MyModule = {};
Elm.Native.MyModule.make = make;