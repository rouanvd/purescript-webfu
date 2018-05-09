'use strict';


exports.mkPromise_ffi = function(executorF) {
  return function() { // Eff wrapper
    var p = new Promise( 
      function(resolveF, rejectF) { 
        var resolveF_eff = function(value) { return resolveF(value)(/*Eff*/); };
        var rejectF_eff = function(error) { return rejectF(error)(/*Eff*/); };
        executorF(resolveF_eff)(rejectF_eff)(/*Eff*/); 
      }
    );
    return p;
  };
};


exports.then_ffi = function(onSuccessF, promise) {
  return function() { // Eff wrapper
    var p = promise.then( function(value) { onSuccessF(value)(/*Eff*/); });
    return p;
  };
};


exports.catch_ffi = function(onRejectF, promise) {
  return function() { // Eff wrapper
    var p = promise.catch( function(err) { onRejectF(err)(/*Eff*/); } );
    return p;
  };
};


exports.finally_ffi = function(onFinallyF, promise) {
  return function() { // Eff wrapper
    var p = promise.finally( function() { onFinallyF(/*Eff*/); } );
    return p;
  };
};


exports.mkReject_ffi = function(err) {
  return function() { // Eff wrapper
    var p = Promise.reject( err );
    return p;
  };
};


exports.mkResolve_ffi = function(value) {
  return function() { // Eff wrapper
    var p = Promise.resolve( value );
    return p;
  };
};


exports.race_ffi = function(promises) {
  return function() { // Eff wrapper
    var p = Promise.race( promises );
    return p;
  };
};


exports.all_ffi = function(promises) {
  return function() { // Eff wrapper
    var p = Promise.all( promises );
    return p;
  };
};

