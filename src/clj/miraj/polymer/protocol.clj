(ns miraj.polymer.protocol
  (:refer-clojure :exclude [list load meta]))

(defprotocol ^{:miraj/protocol? true}
  This)

(defprotocol ^{:miraj/protocol? true}
  Lifecycle
  (created [this])
  (attached [this])
  (detached [this])
  (attributeChanged [this])
  (ready [this]))

;; Event protocols
(defprotocol ^{:miraj/protocol? true}
  Gesture
  (down [this])
  (up [this])
  (tap [this])
  (touch [this]))

