// module Pscid.Keypress

var keypress = require('keypress');

exports.initializeKeypresses = function() {
  keypress(process.stdin);
  process.stdin.setRawMode(true);
  process.stdin.resume();
};

exports.onKeypress = function(cb){
  return function(){
    process.stdin.on('keypress', function (ch, key){
      if (key) {
        if (key.ctrl && key.name == 'c') {
          process.exit();
        }
        cb(key)();
      }
    });
  };
};
