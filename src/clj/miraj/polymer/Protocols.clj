(ns miraj.polymer.Protocols
  (:refer-clojure :exclude [list meta]))
  ;; (:require [miraj.markup :refer [make-resource-fns]]))

(println "loading miraj.polymer")

(defprotocol ^{:co-protocol? true}
  This)

(defprotocol ^{:co-protocol? true}
  Lifecycle
  (created [this])
  (attached [this])
  (detached [this])
  (attributeChanged [this])
  (ready [this]))

(defprotocol ^{:co-protocol? true}
  Http
  (request [rqst])
  (response [rsp])
  (error [e]))

(println "loaded miraj.polymer")
