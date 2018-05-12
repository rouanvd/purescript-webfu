'use strict';


exports.win_fetch_foreign = function (url) {
  return function (w) {

    return function () { // Eff wrapper
      var promise = w.fetch( url );
      return promise;
    };

  };
};

