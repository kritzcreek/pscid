//module Pscid.Options

exports.hasNamedScript = function(name){
  return function(){
    try {
      var pjson = require(process.cwd() + '/package.json');
      if (pjson.scripts && pjson.scripts[name]){
        return true;
      }
      else {
        return false;
      }
    } catch (e) {
      return false;
    }
  };
};
