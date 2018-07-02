'use strict';

exports.preventDefaultImpl = function(unitVal, e) {
  return function() { // Effect wrapper
    e.preventDefault();
    return unitVal;
  };
};
