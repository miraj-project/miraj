(ns polymer.iron
  (:refer-clojure :exclude [list meta])
  (:require [polymer.core :refer [make-polymer-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(def polymer-iron-tags
  ["a11y-announcer"
   "a11y-keys"
   "a11y-keys-behavior"
   "ajax"
   "autogrow-textarea"
   "behaviors"
   "checked-element-behavior"
   "collapse"
   "component-page"
   "doc-viewer"
   "dropdown"
   "fit-behavior"
   "flex-layout"
   "form"
   "form-element-behavior"
   "icon"
   "icons"
   "iconset"
   "iconset-svg"
   "image"
   "input"
   "jsonp-library"
   "label"
   "list"
   "localstorage"
   "media-query"
   "menu-behavior"
   "meta"
   "overlay-behavior"
   "pages"
   "range-behavior"
   "resizable-behavior"
   "selector"
   "signals"
   "swipeable-container"
   "test-helpers"
   "validatable-behavior"
   "validator-behavior"])

(make-polymer-fns "iron" polymer-iron-tags)

