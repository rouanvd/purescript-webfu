'use strict';

////////////////
// Properties //
////////////////
exports.permissionImpl = function() {
    return function() { //Eff wrapper
      return Notification.permission; 
    }
};

exports.langImpl = function(n) {
    return n.lang;
};

exports.mkNotificationImpl = function(s, options) {
    var myNotification = new Notification(s, options);
    return myNotification;
};

exports.bodyImpl = function(n) {
    return n.body;
};

exports.dirImpl = function(n) {
    return n.dir;
};

exports.dataImpl = function(n) {
    return n.data;
};

exports.tagImpl = function(n) {
    return n.tag;
};

exports.iconImpl = function(n) {
    return n.icon;
};

exports.titleImpl = function(n) {
    return n.title;
}

/////////////
// Methods //
/////////////

exports.requestPermissionImpl = function() {
  return Notification.requestPermission();
};

exports.closeImpl = function(n, time) {
    return function() {
      setTimeout(n.close.bind(n), time);
      return {};
    };
};

////////////
// Events //
////////////
