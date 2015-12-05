(ns miraj.core
  (:require [clojure.core.async :as async :refer :all]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.data.xml :as xml]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            ;; [clojure.tools.analyzer.jvm :as ana.jvm]
            ;; [clojure.tools.analyzer.ast :as ast]
            ;; [clojure.tools.analyzer.passes.jvm.emit-form :as e]
            ;; [cljs.analyzer :as ana]
            [slingshot.slingshot :refer [try+ throw+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            ;; [hiccup.core :refer [html]]
            ;; [hiccup.page :refer [html5]]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]])
  (:import [java.io StringReader StringWriter]))

(log/trace "loading")

