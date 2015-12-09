(ns miraj.common
  (:require [miraj.data.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            ;; [slingshot.slingshot :refer [try+ throw+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.keyword-params :refer [keyword-params-request]]
            [ring.middleware.params :refer [params-request]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]]
            [miraj.http.status :as http])
  (:import [java.io StringReader StringWriter]))

(log/trace "loading")

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

(defmacro is-lambda? [f]
  `(let [x# (cond
              (symbol? ~f) (fn? (deref (find-var ~f)))
              (fn? ~f) true
              (= (type ~f) clojure.lang.Cons) (= 'fn* (first ~f))
              (list? ~f) (or (= 'fn (first ~f)) (= 'fn* (first ~f)))
              :else false)]
     ;; (log/trace "is-lambda? " ~f (type ~f) ": " x#)
     x#))

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

;; (defn match-arglist

(defn match-params-to-sig
  [rqst func]
  (log/trace "match-params-to-sig uri: " (:uri rqst) ", fn: " func (type func))
  (log/trace "base uri: " (:miraj-baseuri rqst))
  ;; ring terminology: "params" are incoming, from rqst
  ;; so "args" are formal, defined by the fn signature
  ;; first put all the request params in the ring rqst map
  (let [rqst (keyword-params-request (params-request rqst))

        uri-pfx-nodes (if (:miraj-baseuri rqst)
                        (vec (filter #(not (empty? %)) (str/split (:miraj-baseuri rqst) #"/")))
                        [])
        uri-path-params (vec (filter #(not (empty? %)) (str/split (:uri rqst) #"/")))
        uri-path-params (subvec uri-path-params (count uri-pfx-nodes))
        log (log/trace "uri-pfx-nodes: " uri-pfx-nodes)
        log (log/trace "uri-path-params: " uri-path-params)

        uri-params (:params rqst)
        log (log/trace "uri-params: " uri-params)

        ;; now get the arglist from the fn defn
        ;; sig-args (first (:arglists (meta (find-var func))))
        ;;FIXME support multi-arity fns
        sig-arglists (-> func find-var meta :arglists)
        sig-args (-> func find-var meta :arglists first)
        sig-path-args (filter #(not (or (:? (meta %)) (:?? (meta %)))) sig-args)
        [sig-path-args sig-path-varargs] (split-with #(not= '& %) sig-path-args)
        log (log/trace "sig-path-args: " sig-path-args)
        log (log/trace "sig-path-varargs: " sig-path-varargs)

        required-params (map #(keyword %) (filter #(:? (meta %)) sig-args))
        optional-params (map #(keyword %) (filter #(:?? (meta %)) sig-args))
        log (log/trace "required-params: " required-params)
        log (log/trace "optional-params: " optional-params)]

    (if (empty? sig-path-varargs)
      (if (not= (count uri-path-params) (count sig-path-args))
        ;;FIXME: should response be 404 not found?
        (let [s (str http/bad-request " " (-> http/bad-request http/status :name)
                     ": " (-> http/bad-request http/status :description))]
          ;; (do (log/trace s "  URI path should have exactly " (count sig-path-args) "path nodes"
          ;;                "following base path " (:miraj-baseuri rqst))
              (bad-request! (str s " URI path should have exactly " (count sig-path-args) " path nodes"
                                 " following base path " (:miraj-baseuri rqst))))
        (log/trace "match: uri-path-params & sig-path-args"))

      (if (< (- (count uri-path-params) 1) (count sig-path-args))
        (do (let [s (str http/bad-request " " (-> http/bad-request http/status :name)
                         ": " (-> http/bad-request http/status :description))]
              (log/trace (str s " URI path should have at least " (count sig-path-args) " path nodes."))
              (bad-request!
               (str s "  URI path should have at least " (count sig-path-args) " path nodes."))))
        (log/trace "match: uri-path-params & sig-path-args")))

    (if (not (every? (set (keys uri-params)) required-params))
      (let [s (str http/bad-request " " (-> http/bad-request http/status :name)
                         ": " (-> http/bad-request http/status :description))]
            (bad-request! (str s  "  Missing required body/query param: "
                               (pr-str required-params) " != " (into '() (keys uri-params)))))
      (log/trace "match: uri-params & required-params"))

    (let [required-set (set required-params)
          param-arg-keys (keys uri-params)
          optional-param-args (remove #(contains? required-set %) param-arg-keys)
          log (log/trace "optional-param-args: " optional-param-args)]
    (if (not (every? (set optional-params) optional-param-args))
      (let [s (str http/bad-request " " (-> http/bad-request http/status :name)
                         ": " (-> http/bad-request http/status :description))]
        (bad-request! (str s "  Unknown optional body/query param: "
                         (pr-str optional-params)
                         " != " (pr-str optional-param-args))))
      (log/trace "match: uri-params & optional-params")))

    (let [args (map-indexed
                (fn [i param]
                  (do (log/trace "param: " param (if (meta param) (meta param)))
                     (if (:? (meta param))
                       (do
                           (get uri-params (keyword param)))
                       (if (:?? (meta param))
                         (do
                           (get uri-params (keyword param)))
                         (do (log/trace "path arg " (get uri-path-params i))
                             (get uri-path-params i))))))
                sig-args)]
      (log/trace "args: " args)
      args)))

(defn get-dispatch-entry
  [rqst]
  (log/trace "get-dispatch-entry") ; ": " rqst)
  (let [uri (:uri rqst)
        method (:request-method rqst)
        dispatch-map (get-dispatch-map method)]
    (if-let [dispatch-val (get (deref dispatch-map) uri)]
      (do (log/trace "exact match: " uri dispatch-val)
          [uri dispatch-val])
      (let [pfx-match? (fn [mapentry]
                         (let [uri-str (str (first mapentry))
                               re (re-pattern (if (= "/" uri-str) "/" (str uri-str ".*")))
                               match (re-matches re uri)]
                           (if match (do (log/trace "match: " match " against " re)
                                         true)
                                         ;; [uri-str mapentry])
                               false)))
            matchings (filter pfx-match? (deref dispatch-map))]
        (log/trace uri "matchings: " (pr-str matchings))
        ;; each match is a pair [uri chan], so we can find longest matched uri
        (if (empty? matchings)
          nil
          (do
            (if (> (count matchings) 1)
              (let [longest-match (reduce (fn [longest current]
                                            (if (> (count (first longest))
                                                   (count (first current)))
                                              longest
                                              current))
                                          matchings)]
                (log/trace "longest match: " longest-match)
                longest-match)
              (first matchings))))))))


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
