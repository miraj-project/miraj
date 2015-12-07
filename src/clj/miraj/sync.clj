(ns miraj.sync
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
            [miraj.http.response :refer [bad-request bad-request! not-found]])
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

(defn default-dispatch
  [in-chan behavior]
  (log/trace "default-dispatch")
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
    (let [ch (chan)
          path  (ns-to-path (subs (str chan-kw) 1))]
      (log/trace "NEW CHANNEL: " :get path) ;; ": " ch)
      (swap! mrj/dispatch-map-get
             (fn [old path channel] (assoc old path channel))
             (if (= chan-kw :$) "/" path)
             ch)
      (start-netspace-observer :get chan-kw handler))))
      ;; (start-netspace-observer path ch handler))))

(defn config-polymer-reqs
  [reqs]
  (log/trace "config-polymer-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          pm-path (ns-to-path nmsp)
          pm-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " pm-path)
      (swap! mrj/dispatch-map-get
             (fn [old path channel] (assoc old path channel))
             pm-path pm-chan)
      (start-netspace-observer pm-path pm-chan
                     #(do (log/trace "HANDLING POLYMER RQST: " (:uri %))
                          (let [resp (resource-request % "/")]
                            (if resp
                              resp
                              (do (log/trace "POLYMER RESOURCE NOT FOUND: " (:uri %))
                                  (not-found (:uri %))))))))))

(defn config-js-reqs
  [reqs]
  (log/trace "config-js-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          js-path (str (ns-to-path nmsp) ".*\\.js")
          js-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " js-path)
      (swap! mrj/dispatch-map-get
             (fn [old path ch] (assoc old path ch))
             js-path js-chan)
      (start-netspace-observer js-path js-chan  #(do (log/trace "HANDLING JS RQST: " (:uri %))
                                           (let [resp (resource-request % "scripts/")]
                                             (if resp
                                               resp
                                               (do (log/trace "JS NOT FOUND: " (:uri %))
                                                   (not-found (:uri %))))))))))

(defn config-css-reqs
  [reqs]
  (log/trace "config-css-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          css-path (str (ns-to-path nmsp) ".*\\.css")
          css-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " css-path)
      (swap! mrj/dispatch-map-get
             (fn [old path ch] (assoc old path ch))
             css-path css-chan)
      (start-netspace-observer css-path css-chan
                     #(do (log/trace "HANDLING CSS RQST: " (:uri %))
                          (let [resp (resource-request % "styles/")]
                            ;; :styles.*  #(resource-request % "/")
                            (if resp
                              resp
                              (do (log/trace "CSS NOT FOUND: " (:uri %))
                                  (not-found (:uri %))))))))))

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
(defn demultiplex-rqst-sig
  [params rqst ]
  (let [rqst (keyword-params-request (params-request rqst))
        ;; log (log/trace "demultiplex-rqst-sig: " (pprint-str rqst))
        log (log/trace "demultiplex-rqst-sig uri: " (:uri rqst))
        log (log/trace "demultiplex-rqst-sig params: " params)

  ;; default ring keys
  ;;     (:body :character-encoding :content-length :content-type
  ;;      :headers :protocol :query-string :remote-addr :request-method
  ;;      :scheme :server-name :server-port :ssl-client-cert :uri)


        ;; keywordize params
        ;; params (map #(if (or (:? (meta %)) (:?? (meta %)))
        ;;                (with-meta (keyword %) (meta %)) %)
        ;;             params)
        ;; log (log/trace "params: " params)

        ;; params are def'd, args are incoming from rqst;
        ;;FIXME: deal with base url

        baseuri (vec (filter #(not (empty? %)) (str/split (:miraj-baseuri rqst) #"/")))
        path-args (vec (filter #(not (empty? %)) (str/split (:uri rqst) #"/")))
        path-args (subvec path-args (count baseuri))
        log (log/trace "baseuri: " baseuri)
        log (log/trace "path-args: " path-args)

        path-params (filter #(not (or (:? (meta %)) (:?? (meta %)))) params)
        [path-params varargs] (split-with #(not= '& %) path-params)
        log (log/trace "path-params: " path-params)
        log (log/trace "varargs: " varargs)

        param-args (:params rqst)
        log (log/trace "param-args: " param-args)
        required-params (map #(keyword %) (filter #(:? (meta %)) params))
        optional-params (map #(keyword %) (filter #(:?? (meta %)) params))
        log (log/trace "required-params: " required-params)
        log (log/trace "optional-params: " optional-params)]

    (if (empty? varargs)
      (if (not= (count path-args) (count path-params))
        (do (log/trace (str "Error: url path should have exactly " (count path-params) " path nodes."))
            (bad-request! (str "Error: url path should have exactly " (count path-params) " path nodes following base path " (:miraj-baseuri rqst))))
        (log/trace "match: path-args & path-params"))
      (if (< (- (count path-args) 1) (count path-params))
        (do (log/trace (str "Error: url path should have at least " (count path-params) " path nodes."))
            (bad-request!
             (str "Error: url path should have at least " (count path-params) " path nodes.")))
        (log/trace "match: path-args & path-params")))

    (if (not (every? (set (keys param-args)) required-params))
      (bad-request! (str "Error: missing required body/query param: "
                         (pr-str required-params) " != " (into '() (keys param-args))))
      (log/trace "match: param-args & required-params"))

    (let [required-set (set required-params)
          param-arg-keys (keys param-args)
          optional-param-args (remove #(contains? required-set %) param-arg-keys)
          log (log/trace "optional-param-args: " optional-param-args)]
    (if (not (every? (set optional-params) optional-param-args))
      (bad-request! (str "Error: unknown optional body/query param: "
                         (pr-str optional-params)
                         " != " (pr-str optional-param-args)))
      (log/trace "match: param-args & optional-params")))

    (let [args (map-indexed
                (fn [i param]
                  (do (log/trace "param: " param (if (meta param) (meta param)))
                     (if (:? (meta param))
                       (do
                           (get param-args (keyword param)))
                       (if (:?? (meta param))
                         (do
                           (get param-args (keyword param)))
                         (do (log/trace "path arg " (get path-args i))
                             (get path-args i))))))
                params)]
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
    ;;               param-args)]
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
    (default-dispatch default-chan (if (list? co-fn) (eval co-fn) co-fn))
    (let [ch (chan)
          path (if (= netspace-sym '$)
                 "/"
                 (ns-to-path (str netspace-sym)))
          dispatch-map (mrj/get-dispatch-map method)]
      (log/trace "NEW CHANNEL: " method path) ;;  ": " ch " on dm: " dispatch-map)
      (swap! dispatch-map
             (fn [old path channel] (assoc old path channel))
             path ch)
      [path ch co-fn])))
;;      (start-netspace-observer path ch co-fn))))

(defn start-netspace-observer
  [method netspace-sym co-fn]
  (let [[path in-chan behavior] (configure-netspace! method netspace-sym co-fn)]
    (log/trace "start-netspace-observer for: " path) ;; in-chan  behavior (type behavior) " fn? " (fn? behavior))
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

      (log/trace "co-fn? " cofn? behavior)

      (go ;(log/trace "launching netchan " in-chan beh)
        (while true
          (let [rqst (<! in-chan)
                uri (:uri rqst)]
            ;; (log/trace "resuming netchan: " (str in-chan "->" 'http-resp-chan) " handling: " uri)
            ;; note: we don't actually do anything with the request
            ;; (log/trace "http-resp chan: " http-resp-chan)
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
                  (let [v (find-var behavior)
                        m (meta v)
                        args (try+ (demultiplex-rqst-sig (first (:arglists m)) rqst)
                                   (catch [:type :miraj.http.response/response]
                                       {:keys [response]}
                                     ;; (log/trace "caught exc " (:status response))
                                     (>! http-resp-chan response)
                                     nil))]
                    (if (not (nil? args))
                      (do
                        (log/trace "calling (" (:name m) args "), for " (:uri rqst))
                        (if-let [res (apply (deref v) args)]
                          (do (log/trace (:name m) " says: " res)
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
                ))))))))

(defn start-http-observer
  [] ;;[disp-map]
  (log/trace "start-http-observer")
  (go (while true
         (let [rqst (<! http-rqst-chan)
               log (log/trace "rqst: " rqst)
               uri (:uri rqst)
               method (:request-method rqst)
               dispatch-map (mrj/get-dispatch-map method)
               log (log/trace "resuming HTTP dispatch for " method uri)
               ;; log (mrj/dump-dispatch-map method)
               [drqst dchan]
               (if-let [to-chan (get (deref dispatch-map) uri)]
                 (do (log/trace "exact match: " uri to-chan)
                     [(assoc rqst :miraj-baseuri uri) to-chan])
                 (let [pred (fn [mapentry]
                              (let [uri-str (str (first mapentry))
                                    ;;FIXME: currently "/" match is absolute, no /*
                                    re (re-pattern (if (= "/" uri-str) "/" (str uri-str ".*")))
                                    match (re-matches re uri)]
                                (if match
                                  (do (log/trace "match: " match " against " re)
                                      [uri-str mapentry])
                                  nil)))
                       matchings (filter pred (deref dispatch-map))]
                   (log/trace uri "matchings: " (pr-str matchings))
                   (if (empty? matchings)
                     [rqst default-chan]
                     (do
                       (if (> (count matchings) 1)
                         (let [m (reduce (fn [cum match]
                                           (if (> (count (first cum))
                                                  (count (first match)))
                                             cum match))
                                         matchings)
                               log (log/trace "longest match: " m)
                               to-chan (last m)
                               baseuri (first m)]
                           (log/trace "to-chan: " to-chan)
                           (log/trace "baseuri: " baseuri)
                           [(assoc rqst :miraj-baseuri baseuri) to-chan])
                         (let [to-chan (last (first matchings))
                               baseuri (first (first matchings))]
                           (log/trace "to-chan: " to-chan)
                           (log/trace "baseuri: " baseuri)
                           [(assoc rqst :miraj-baseuri baseuri) to-chan]))))))]
          ;; (log/trace "DEFAULT CHAN: " default-chan)
          ;; (log/trace "DISPATCHCHAN: " dchan)
          (log/trace "CHAN dispatching uri: " uri)
          (>! dchan drqst)))))

(defn start [rqst]
  (log/trace "dispatching http rqst: " (:uri rqst))
  ;; find matching route in dispatch table
  ;; call fn
  (go (>! http-rqst-chan rqst))
  (let [r (<!! http-resp-chan)]
    ;; (log/trace "responding " r)
    r))

(log/trace "loaded")
