//module Pscid.Options

exports.hasNamedScript = function(name){
  return function(){
    try {
      var pjson = require(process.cwd() + '/package.json');
      return pjson.scripts && pjson.scripts[name];
    } catch (e) {
      return false;
    }
  };
};

exports.glob = function(pattern) {
  return function() {
    return require('glob').sync(pattern);
  };
};

exports.version = function() {
  // This one references pscid's package.json
  var pjson = require('../../package.json');
  if(!pjson) {
    return "Unknown";
  } else {
    return pjson.version;
  }
}
