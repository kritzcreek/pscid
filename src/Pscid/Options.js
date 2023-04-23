//module Pscid.Options

import { createRequire } from 'module';
const require = createRequire(import.meta.url);

export let hasNamedScript = function (name) {
  return function () {
    try {
      var pjson = require(process.cwd() + '/package.json');
      return pjson.scripts && pjson.scripts[name];
    } catch (e) {
      return false;
    }
  };
};

export let glob = function (pattern) {
  return function () {
    return require('glob').sync(pattern);
  };
};

export let version = function () {
  // This one references pscid's package.json
  var pjson = require('../../package.json');
  if (!pjson) {
    return 'Unknown';
  } else {
    return pjson.version;
  }
};
