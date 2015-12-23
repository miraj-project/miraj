(ns miraj.bad-hello
  "docstring here"
  (:require [miraj.core :refer :all]
            [polymer.paper :as paper :only [button menu toast toolbar]]
            [polymer.iron :as iron] ;; :refer :all :exclude [behaviors input]]
            [polymer.font :as fnt :only [roboto]]
            [miraj.html :as h] ;; :refer :all :exclude [button menu progress form]]
            [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.test]
            [clojure.data.xml]
            [clojure.data.xml.test-utils :refer [test-stream lazy-parse*]]))

;; RuntimeException: co-routines can only be defined within a co-namespace.

(co-routine main ;;defn main
  []
  (h/h1 "Hello world"))
