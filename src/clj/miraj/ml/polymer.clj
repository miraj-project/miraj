(ns miraj.ml.polymer
  (:refer-clojure :exclude [map meta time])
  (:require [miraj.ml.core :refer [make-fns]]))

(def polymer-tags
  ["dom-module" "dom-repeat"])

(make-fns polymer-tags)
