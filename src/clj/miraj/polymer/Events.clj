(ns miraj.polymer.Events
  (:refer-clojure :exclude [load]))

;; (defprotocol ^{:resource-type :polymer-events
;;                :resource-pfx "bower_components"}
;;   Lifecycle
;;   (created [this])
;;   (attached [this])
;;   (detached [this])
;;   (attribute-changed [this]))

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Gesture
  (down [this])
  (up [this])
  (tap [this])
  (touch [this]))

;; Event types
;; https://developer.mozilla.org/en-US/docs/Web/Events

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  UI
  (abort [x])
  (error [x])
  (load [x])
  (resize [x])
  (scroll [x])
  (select [x])
  (unload [x]))

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Focus
  (blur [x])
  (focus [x])
  (focusin [x])
  (focusout [x]))


(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Mouse
  (click [x])
  (dblclick [x])
  (mousedown [x])
  (mouseenter [x])
  (mouseleave [x])
  (mousemove [x])
  (mouseout [x])
  (mouseover [x])
  (mouseup [x]))

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Progress)

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Animation)

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  Clipboard)

(defprotocol ^{:resource-type :polymer-events
               :resource-pfx "bower_components"}
  SpeechSynthesis
  (soundstart [x])
  (soundend [x]))

