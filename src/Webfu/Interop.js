'use strict';

exports.readProp = function (prop, obj) {
  return obj[prop];
};


exports.runEffMethod0Impl = function (unitVal, method, obj) {
  return function () { // Effect wrapper
    obj[method]();
    return unitVal;
  };
};
