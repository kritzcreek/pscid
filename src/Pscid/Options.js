//module Pscid.Options

exports.hasBuildScript = function(){
  try {
    var pjson = require(process.cwd() + '/package.json');
    if (pjson.scripts && pjson.scripts.build){
      return true;
    }
    else {
      return false;
    }
  } catch (e) {
    return false;
  }
};
