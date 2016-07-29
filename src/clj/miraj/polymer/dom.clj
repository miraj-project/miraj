(ns miraj.polymer.dom
  (:refer-clojure :exclude [for])
  (:require [miraj.markup :refer [make-resource-fns]]))

(alter-meta! *ns* (fn [m] (assoc m :co-ns true)))

(def dom-tags
  ; fn-tag,  elt-tag,  elt-uri,  docstring
  [['bind :dom-bind "" "tag: <dom-bind>"]
   ['if :dom-if "" "tag: <dom-if>"]
   ['for :dom-repeat "" "tag: <dom-repeat>"]
   ['module :dom-module "" "tag: <dom-module>"]
   ['content :content "" "tag: <content>"]])

(make-resource-fns :html dom-tags)
