(ns miraj.polymer.platinum
  (:require [miraj.polymer.core :as p :refer [make-fns]]))

(def polymer-platinum-tags
  ["bluetooth"
   "https-redirect"
   "push-messaging"
   "sw"])

(p/make-fns "platinum" polymer-platinum-tags)
