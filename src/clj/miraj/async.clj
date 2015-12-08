(ns miraj.async
  (:require [clojure.core.async :as async :refer :all :exclude [into map merge partition reduce take]]
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
            [miraj.common :as mrj]
            [miraj.data.xml :as xml]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]]
            [miraj.http.status :as http])
  (:import [java.io StringReader StringWriter]))

(log/trace "loading")

;; dispatch-map takes URIs to input channels
;; (defonce dispatch-map-get (atom {}))
;; (defonce dispatch-map-head (atom {}))
;; (defonce dispatch-map-post (atom {}))
;; (defonce dispatch-map-put (atom {}))

(defonce http-rqst-chan (chan 20))
(defonce http-resp-chan (chan 20))
(defonce default-chan (chan 20))

;;TODO: support config of base path
(def polymer-map
  {:polymer #(resource-request % "/")
   :scripts #(resource-request % "/")
   :styles  #(resource-request % "/")
   :themes  #(resource-request % "/")})

(declare start-netspace-observer)

(defn ns-to-path
  [ns]
  (let [s (str/replace (str ns) #"\.|-|\*" {"." "/" "-" "_" "*" ".*"})]
    (if (= \/ (first s)) s (str "/" s))))

(defn start-default-observer
  [in-chan behavior]
  (log/trace "starting start-default-observer")
  (go ;(log/trace "launching default chan")
    (while true
      (let [rqst (<! in-chan)
            uri (:uri rqst)]
        (log/trace "default dispatch on: " uri)
        (>! http-resp-chan (behavior rqst))))))

(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

(defn path-to-ns
  [ns]
  (subs (str/replace (str ns) "/" ".") 1))

;;FIXME: only include polymer stuff on demand
(defn config-polymer-defaults
  []
  (doseq [[chan-kw handler] polymer-map]
    (start-netspace-observer :get (subs (str chan-kw) 1) handler))
  #_(mrj/dump-dispatch-map :get))

(defn config-polymer-reqs
  [reqs]
  (log/trace "config-polymer-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          pm-path (ns-to-path nmsp)
          pm-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " pm-path)
      (start-netspace-observer :get nmsp
                               #(do (log/trace "HANDLING POLYMER RQST: " (:uri %))
                                    (let [resp (resource-request % "/")]
                                      (if resp
                                        resp
                                        (do (log/trace "POLYMER RESOURCE NOT FOUND: " (:uri %))
                                            (not-found (:uri %)))))))))
  #_(mrj/dump-dispatch-map :get))

(defn config-js-reqs
  [reqs]
  (log/trace "config-js-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          js-path (str nmsp ".*js")] ;;FIXME regex syntax
          ;; js-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " js-path)
      (start-netspace-observer :get (symbol js-path)
                               #(do (log/trace "HANDLING JS RQST: " (:uri %))
                                    (let [resp (resource-request % "scripts/")]
                                      (if resp
                                        resp
                                        (do (log/trace "JS NOT FOUND: " (:uri %))
                                            (not-found (:uri %)))))))))
  #_(mrj/dump-dispatch-map :get))

(defn config-css-reqs
  [reqs]
  (log/trace "config-css-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          css-path (str nmsp ".*css")] ;;FIXME: regex syntax
          ;; css-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " css-path)
      (start-netspace-observer :get (symbol css-path)
                     #(do (log/trace "HANDLING CSS RQST: " (:uri %))
                          (let [resp (resource-request % "styles/")]
                            ;; :styles.*  #(resource-request % "/")
                            (if resp
                              resp
                              (do (log/trace "CSS NOT FOUND: " (:uri %))
                                  (not-found (:uri %)))))))))
  #_(mrj/dump-dispatch-map :get))

(defn configure-namespace
  [nm refs]
  (log/trace "configure-namespace " nm)
  (let [ref-map (into {} (clojure.core/map
                          #(identity [(first %) (rest %)]) refs))
        clj-reqs (:require ref-map)
        html-reqs (:html ref-map)
        js-reqs (filter #(some #{:js} %) html-reqs)
        css-reqs (filter #(some #{:css} %) html-reqs)
        libs (filter #(not (some #{:js :css} %)) clj-reqs)

        polymer-reqs (:polymer ref-map)
        ;; pm-reqs (filter #(.startsWith (str (first %)) "polymer.") polymer-reqs)

        required (list (apply list ':require (concat libs polymer-reqs)))]
    ;; (println "REFS: " refs)
    (println "REQUIRED: " required)
    (if (not (nil? polymer-reqs))
      (do (log/trace "POLYMER reqs: " polymer-reqs)
          (config-polymer-reqs polymer-reqs)))

    (if (not (nil? js-reqs))
      (do (log/trace "JS reqs: " js-reqs)
          (config-js-reqs js-reqs)))

    (if (not (nil? css-reqs))
      (do (log/trace "CSS reqs: " css-reqs)
          (config-css-reqs css-reqs)))

    (config-polymer-defaults)
    required))


;FIXME: validation.  count of plain args must match count of nodes, etc.
;;FIXME: account for base url nodecount
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

    ;; (let [u (:uri rqst)]
    ;;   (let [args (map-indexed
    ;;               (fn [i arg]
    ;;                 (log/trace "arg " i ": " arg)
    ;;                 (if (= \& (first (str arg)))
    ;;                   (do (log/trace "found url parm")
    ;;                       nil)
    ;;                   (let [node (get-path-node i u)]
    ;;                     (log/trace "node: " node)
    ;;                     node)))
    ;;               uri-params)]
    ;;     (log/trace "ARGS: " args)
    ;;     args))))

(defn get-body
  [ns component]
  ;; (log/trace "get-body ns: " ns)
  ;; (log/trace "get-body: " component (type component))
  ;; (log/trace "get-body class: " (class component) " / " (type component))
    ;; for dev, always reload
  ;; (log/trace "reloading ns " ns)
  ;; (require (ns-name ns) :reload)
  (let [body (if (fn? component) (component)
                 (if (symbol? component) ((resolve component))))]
    ;; (log/trace "body: " body (type body))
    (if (= :body (:tag body))
      body
      (h/body {:unresolved "unresolved"} body))))

;; (defmacro activate [component]
(defn activate [component]
  (log/trace "activate: " component (type component))
  (cond
    (symbol? component)
    (do (log/trace "activating symbol")
        (let [ns-sym (symbol (namespace component))
              ;; log (log/trace "ns-sym: " ns-sym)
              ns (find-ns ns-sym)
              ;; log (log/trace "ns: " ns)
              nm (name component)]
          (if (nil? ns) (do (log/trace "requiring ns " ns-sym) (require ns-sym)))
          (if (not (:co-fn (meta (find-var component))))
            (throw (RuntimeException. (str "only co-functions can be activated."))))
          (let [ns (find-ns (symbol (namespace component)))
                ;; log (log/trace "ACTIVATE ns: " ns)
                preamble (if-let [cofn (:co-fn (meta ns))]
                           cofn
                           (throw (RuntimeException.
                                   (str "co-functions must be defined in a co-namespace."))))]
            ;; (log/trace "ACTIVATE PREAMBLE: " preamble)
            (let [body# (get-body ns component)
                  ;; log# (log/trace "ACTIVATE BODY: " body#)
                  tree# (h/html preamble body#)]
              ;; (log/trace "TREE: " tree#)
              tree#))))

    (var? component)
    (do (log/trace "activating var meta:" (meta component))
        (if (not (:co-fn (meta component)))
          (throw (RuntimeException. (str "only co-functions can be activated.")))
          (log/trace "found co-fn"))
        (let [ns (:ns (meta component))
              ;; log (log/trace "found ns: " (meta ns))
              preamble (if (not (:co-ns (meta ns)))
                         (throw (RuntimeException. (str "co-functions must be defined in a co-namespace.")))
                         (:co-fn (meta ns)))]
          (log/trace "preamble: " preamble)
          ;;`(do (log/trace "activating " (str (ns-name ~ns) "/" ~nm))
          (let [body# (get-body ns component)
                ;; log# (log/trace "body: " body#)
                tree# (h/html preamble body#)]
            ;; (log/trace "tree: " tree#)
            tree#)))
    :else
    (do (log/trace "activating other: " (type component)))))

(defn configure-netspace!
  [method netspace-sym co-fn]
  (log/trace "configure-netspace! " method netspace-sym (type netspace-sym) co-fn (type co-fn))
  ;; (doseq [[netspace-sym co-fn] disp-map]
  (if (= '* netspace-sym)
    [nil default-chan co-fn]
    (let [ch (chan)
          path (if (= netspace-sym '$)
                 "/"
                 ;;FIXME: path regex
                 (ns-to-path (str netspace-sym)))
          dispatch-map (mrj/get-dispatch-map method)]
      (log/trace "NEW CHANNEL: " method path) ;;  ": " ch " on dm: " dispatch-map)
      (swap! dispatch-map
             (fn [old path channel] (assoc old path channel))
             path ch)
      [path ch co-fn])))
      ;; (start-netspace-observer path ch co-fn))))

(defn start-netspace-observer
  [method netspace-sym co-fn] ;;FIXME: add type param, e.g. :css, :js
  (log/trace "start-netspace-observer: " method netspace-sym co-fn)
  (let [[path in-chan behavior] (configure-netspace! method netspace-sym co-fn)]
    (if (nil? path)
      (start-default-observer default-chan (if (list? co-fn) (eval co-fn) co-fn))
      ;; [chan & behavior]
      ;; creates a pair of gochans
      (let [cofn? (if (symbol? behavior)
                    (:co-fn (meta (find-var behavior)))
                    (if (var? behavior)
                      (:co-fn (meta behavior))
                      false))

            beh (cond
                  (list? behavior) (eval behavior) ;;FIXME
                  (symbol? behavior) behavior
                  (fn? behavior) behavior
                  :else (throw (Exception. "dispatch table entries must be [symbol symbol] or [symbol list] or [symbol fn] pairs")))]

        ;; (log/trace "co-fn? " cofn? behavior)
        (go (log/trace "observing netspace " in-chan beh)
          (while true
            (let [rqst (<! in-chan)
                  uri (:uri rqst)]
              ;; (log/trace "resuming netchan: " (str in-chan "->" 'http-resp-chan) " handling: " uri)
              (if cofn?
                (do (log/trace "dispatching co-fn " uri (:miraj-baseuri rqst))
                    (if (= (:uri rqst) (:miraj-baseuri rqst))
                      (do (log/trace "activating co-fn " beh) ;; (type beh))
                          (let [body (activate beh)
                                r (xml/serialize :html body)]
                            ;; (log/trace "RESULT: " r)
                            (>! http-resp-chan (response r))))
                      (>! http-resp-chan (not-found))))
                (do
                  (if (symbol? behavior)
                    (let [args (try+ (match-params-to-sig rqst behavior)
                                     (catch [:type :miraj.http.response/response]
                                         {:keys [response]}
                                       ;; (log/trace "caught exc " (:status response))
                                       (>! http-resp-chan response)
                                       nil))]
                      (if (not (nil? args))
                        (do
                          (log/trace "calling ("
                                     (-> behavior find-var meta :name) args
                                     "), for " (:uri rqst))
                          (if-let [res (apply (deref (-> behavior find-var)) args)]
                            (do (log/trace (-> behavior find-var meta :name) " says: " res)
                                (>! http-resp-chan res))
                            (>! http-resp-chan (not-found))))))
                    (do (log/trace "applying lambda " (:uri rqst) beh)
                        (log/trace "type lambda " (type beh))
                        (log/trace "class lambda " (class beh))
                        (log/trace "meta lambda " (meta beh))
                        (>! http-resp-chan
                            (if-let [res (beh rqst)]
                              (do ;;(log/trace "if-let " res)
                                res)
                              (do ;;(log/trace "not found ")
                                (not-found))))))
                  )))))))))

(defn get-dispatch-entry
  [rqst]
  (log/trace "get-dispatch-entry") ; ": " rqst)
  (let [uri (:uri rqst)
        method (:request-method rqst)
        dispatch-map (mrj/get-dispatch-map method)]
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
              ;;     to-chan (last longest-match)
              ;;     baseuri (first longest-match)]
              ;; [(assoc rqst :miraj-baseuri baseuri) to-chan])
              (first matchings))))))))
              ;; (let [to-chan (last (first matchings))
              ;;       baseuri (first (first matchings))]
              ;;   [(assoc rqst :miraj-baseuri baseuri) to-chan]))))))))

(defn start-http-observer
  [] ;;[disp-map]
  (log/trace "start-http-observer")
  (go (while true
        (let [rqst (<! http-rqst-chan)]
          (log/trace "http dispatching rqst: " (:request-method rqst) (:uri rqst))
          (if-let [dispatch-val (get-dispatch-entry rqst)]
            (do (log/trace "dispatch val: " dispatch-val)
                 (log/trace "dispatch chan: " (last dispatch-val))
                (>! (last dispatch-val) (assoc rqst :miraj-baseuri (first dispatch-val))))
            (>! default-chan (assoc rqst :miraj-baseuri nil)))))))

(defn start [rqst]
  (log/trace "start http rqst: " (:uri rqst))
  (go (>! http-rqst-chan rqst))
  (let [resp (<!! http-resp-chan)]
    ;; (log/trace "responding " resp)
    resp))

(log/trace "loaded")
