//module Main

var gaze = require('gaze');

exports.gaze = function(globs, cb){
  gaze(globs, function(err, watcher) {
    // Files have all started watching
    // watcher === this

    // Get all watched files
    var watched = watcher.watched();

    watcher.on('changed', function(filepath) {
      cb(filepath)();
    });

    watcher.on('added', function(filepath) {
      cb(filepath)();
    });
  });
};
