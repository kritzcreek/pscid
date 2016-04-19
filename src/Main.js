//module Main

var gaze = require('gaze');

exports.gaze = function(glob, cb){
  gaze(glob, function(err, watcher) {
    // Files have all started watching
    // watcher === this

    // Get all watched files
    var watched = watcher.watched();

    watcher.on('changed', function(filepath) {
      console.log(filepath)
      cb(filepath)();
    });
  });
};

exports.clearConsole = function(){
  process.stdout.write('\033c');
};
