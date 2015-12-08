(ns miraj.common
  (:require [clojure.core.async :as async :refer :all :exclude [into map merge partition reduce take]]
            [miraj.data.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            [slingshot.slingshot :refer [try+ throw+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.keyword-params :refer [keyword-params-request]]
            [ring.middleware.params :refer [params-request]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]])
  (:import [java.io StringReader StringWriter]))

(log/trace "loading")

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

;; dispatch-map takes URIs to input channels
(defonce dispatch-map-get (atom {}))
(defonce dispatch-map-head (atom {}))
(defonce dispatch-map-post (atom {}))
(defonce dispatch-map-put (atom {}))

;;FIXME: implement dispatch-map using protocols/interfaces, e.g. IPersistentMap
(defn get-dispatch-map
  [method]
  ;; (log/trace "get-dispatch-map: " method)
  (condp = method
    :get dispatch-map-get
    :head dispatch-map-head
    :post dispatch-map-post
    :put dispatch-map-put
    (throw (Exception. "bad HTTP method: " method))))

(defn dump-dispatch-map
  [& method]
  (log/trace "dump-dispatch-map " method)
  (if (empty? method)
    (doseq [method [:get :head :post :put]]
      (log/trace "dispatch-map " method ": "
                 (pprint-str (into (sorted-map) (deref (get-dispatch-map method))))))
    (let [dm (get-dispatch-map (first method))]
      (log/trace "dispatch-map " method ": "
                 (pprint-str (into (sorted-map) (deref dm)))))))



;; some of the cljs stuff is borrowed from
;;  http://swannodette.github.io/2014/01/14/clojurescript-analysis--compilation/
;;  https://github.com/swannodette/hello-cljsc
;; cljs compile
(def user-env '{:ns {:name foo.bar} :locals {}})

;; A simple helper which allows us to read ClojureScript source from a string
;; instead of having to bother with files.
(defn string-reader [s]
  (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. s)))

;; A simple helper to emit ClojureScript compiled to JavaScript
;; as a string.
;; (defn emit-str [ast]
;;   (with-out-str (c/emit ast)))

;; A simple helper that takes a stream and returns a lazy sequences of
;; read forms.
;; (defn forms-seq [stream]
;;   (let [rdr (readers/indexing-push-back-reader stream 1)
;;         forms-seq* (fn forms-seq* []
;;                      (lazy-seq
;;                       (if-let [form (reader/read rdr nil nil)]
;;                         (cons form (forms-seq*)))))]
;;     (forms-seq*)))

;; ;; A helper to just read the first s-expression
;; (defn read1 [str]
;;   (first (forms-seq (string-reader str))))

;; (read1 "[1 2 3]")

;; (let [form (read1 "[1 2 3]")]
;;   (pp/pprint (ana/analyze user-env form)))

;; This will pretty print an :invoke AST node.
;; (let [form (read1 "(foo 1)")]
;;   (pp/pprint (ana/analyze user-env form)))

;; (first (read1 "(if x true false)"))

;; (let [form (read1 "(if x true false)")]
;;   (pp/pprint (ana/parse (first form) user-env form nil nil)))

;; (let [form (read1 "(if x true false)")]
;;   (pp/pprint (ana/analyze user-env form)))

;; ;; To compile an AST node to JavaScript we just call cljs.compiler/emit
;; ;; with an AST node as the argument. Click on the output box to expand it.
;; (let [form (read1 "(if x true false)")]
;;   (with-out-str (c/emit (ana/analyze user-env form))))

;; ;; Pretty simple! try different things!
;; (let [form (read1 "(fn [a b] (+ a b))")]
;;   (with-out-str (c/emit (ana/analyze user-env form))))

;; (defmacro cljs->js [form]
;;   (let [form# (read1 (str form))]
;; ;;    (println "form# " form#)
;;     (with-out-str (c/emit (ana/analyze user-env form#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; (clojure.core/assert-args
  ;;    (vector? seq-exprs) "a vector for its binding"
  ;;    (even? (count seq-exprs)) "an even number of forms in binding vector")

  ;; (defmacro handle-params
  ;;   [params]
  ;;   (log/trace "handle-params: " params)
  ;;   (let [parms (vec (flatten (merge []
  ;;                                    (for [param params]
  ;;                                      [param (keyword param)]))))]
  ;;     (log/trace "new params: " parms)
  ;;     ;; (let [letparms (list 'let parms)]
  ;;     ;;   (log/trace "let params: " letparms)
  ;;     parms))


(log/trace "loaded")
