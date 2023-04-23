//module Pscid.Options

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

export function hasNamedScript(name) {
  return function () {
    try {
      var pjson = require(process.cwd() + '/package.json');
      return pjson.scripts && pjson.scripts[name];
    } catch (e) {
      return false;
    }
  };
}

export function glob(pattern) {
  return function () {
    return require('glob').sync(pattern);
  };
}

export function version() {
  // This one references pscid's package.json
  var pjson = require('../../package.json');
  if (!pjson) {
    return 'Unknown';
  } else {
    return pjson.version;
  }
}
