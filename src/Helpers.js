"use strict";

exports.setEffectAllowed = function(effect) {
  return function(dragEvent) {
    return function () {
        dragEvent.dataTransfer.effectAllowed = effect;
    }
  };
};

exports.setDropEffect = function(effect) {
  return function(dragEvent) {
    return function () {
        dragEvent.dataTransfer.dropEffect = effect;
    }
  };
};

exports.isInput = function(dragEvent) {
  return function () {
    return document.elementFromPoint(dragEvent.clientX,dragEvent.clientY).tagName == "INPUT";
  };
};

exports.inputValue = function(event) {
  return function () {
    return event.target.value;
  };
};

exports.setValue = function(event) {
  return function (string) {
    return function () {
      event.target.value = string;
    };
  };
};