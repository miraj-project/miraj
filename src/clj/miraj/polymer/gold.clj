(ns miraj.polymer.gold
  (:require [miraj.polymer.core :as p :refer [make-fns]]))

(def polymer-gold-tags
  ["cc-cvc-input"
   "cc-expiration-input"
   "cc-input"
   "email-input"
   "phone-input"
   "zip-input"])

(p/make-fns "gold" polymer-gold-tags)
