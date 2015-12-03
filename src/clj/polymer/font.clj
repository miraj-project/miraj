(ns polymer.font
  (:require [polymer.core :refer [make-polymer-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

;; (def polymer-font-tags
;;   ["roboto"])

;; (make-polymer-fns "font" polymer-font-tags)
