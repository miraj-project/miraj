(ns polymer.paper
  (:refer-clojure :exclude [map meta time])
  (:require [polymer :refer [make-polymer-fns]]))

(println "loading polymer.paper")

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(defonce polymer-paper-tags
  ["badge"
   "behaviors"
   "button"
   "card"
   "checkbox"
   "dialog"
   "dialog-behavior"
   "dialog-scrollable"
   "drawer-panel"
   "dropdown-menu"
   "elements"
   "fab"
   "header-panel"
   "icon-button"
   "input"
   "item"
   "linear-progress"
   "listbox"
   "material"
   "menu"
   "menu-button"
   "progress"
   "radio-button"
   "radio-group"
   "ripple"
   "scroll-header-panel"
   "slider"
   "spinner"
   "styles"
   "tabs"
   "toast"
   "toggle-button"
   "toolbar"
   "tooltip"])

(make-polymer-fns "paper" polymer-paper-tags)

