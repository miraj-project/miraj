(ns polymer.platinum
  (:require [polymer :refer [make-polymer-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(def polymer-platinum-tags
  ["bluetooth"
   "https-redirect"
   "push-messaging"
   "sw"])

(make-polymer-fns "platinum" polymer-platinum-tags)
