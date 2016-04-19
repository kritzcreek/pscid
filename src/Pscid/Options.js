//module Pscid.Options

exports.getBuildScriptImpl = function(nothing, just){
  try {
    var pjson = require(process.cwd() + '/package.json');
    if (pjson.scripts && pjson.scripts.build){
      return just(pjson.scripts.build);
    }
    else {
      return nothing;
    }

  } catch (e) {
    return nothing;
  }

};
