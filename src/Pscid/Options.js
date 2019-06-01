//module Pscid.Options

exports.isSpagoProject = function(){
  return function(){
    try {
      return fs.existsSync(process.cwd() + '/spago.dhall');
    } catch (e) {
      return false;
    }
  }
}

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
