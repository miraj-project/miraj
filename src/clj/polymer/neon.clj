(ns polymer.neon
  (:require [polymer :refer [make-polymer-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(def polymer-neon-tags
  ["animation"])

(make-polymer-fns "neon" polymer-neon-tags)
