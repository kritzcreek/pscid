// module Pscid.Keypress

import keypress from 'keypress';

export let initializeKeypresses = function () {
  keypress(process.stdin);
  process.stdin.setRawMode(true);
  process.stdin.resume();
};

export let onKeypress = function (cb) {
  return function () {
    process.stdin.on('keypress', function (ch, key) {
      if (key) {
        cb(key)();
      }
    });
  };
};
