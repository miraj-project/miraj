(ns polymer.gold
  (:require [polymer.core :refer [make-polymer-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(def polymer-gold-tags
  ["cc-cvc-input"
   "cc-expiration-input"
   "cc-input"
   "email-input"
   "phone-input"
   "zip-input"])

(make-polymer-fns "gold" polymer-gold-tags)
