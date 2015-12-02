(ns miraj.polymer.paper
  (:refer-clojure :exclude [map meta time])
  (:require [miraj.polymer.core :as p :refer [make-fns]]))

(def polymer-paper-tags
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

(p/make-fns "paper" polymer-paper-tags)

