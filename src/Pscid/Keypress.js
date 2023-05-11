// module Pscid.Keypress

import keypress from 'keypress';

export function initializeKeypresses() {
  keypress(process.stdin);
  process.stdin.setRawMode(true);
  process.stdin.resume();
}

export function onKeypress(cb) {
  return function () {
    process.stdin.on('keypress', function (ch, key) {
      if (key) {
        cb(key)();
      }
    });
  };
}
