//module Main

import gaze_ from 'gaze';

export function gaze(globs, cb) {
  gaze_(globs, { follow: true }, function (err, watcher) {
    // Files have all started watching
    // watcher === this

    // Get all watched files
    var watched = watcher.watched();

    watcher.on('changed', function (filepath) {
      cb(filepath)();
    });

    watcher.on('added', function (filepath) {
      cb(filepath)();
    });
  });
}
