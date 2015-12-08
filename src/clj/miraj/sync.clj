(ns miraj.sync
  (:require [clojure.core.async :as async :refer :all :exclude [into map merge partition reduce take]]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            [slingshot.slingshot :refer [try+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.keyword-params :refer [keyword-params-request]]
            [ring.middleware.params :refer [params-request]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            [miraj.common :as mcomm]
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

(declare configure-netspace!)

(defn ns-to-path
  [ns]
  (let [s (str/replace (str ns) #"\.|-|\*" {"." "/" "-" "_" "*" ".*"})]
    (if (= \/ (first s)) s (str "/" s))))

(defn default-observer [rqst]
  (throw (Exception. "invoked dummy default-observer")))
;; (defn start-default-observer
;;   [in-chan behavior]
;;   (log/trace "start-default-observer: " behavior)
;;   (go ;(log/trace "launching default chan")
;;     (while true
;;       (let [rqst (<! in-chan)
;;             uri (:uri rqst)]
;;         (log/trace "default dispatch on: " uri)
;;         (>! http-resp-chan (behavior rqst))))))

(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

(defn path-to-ns
  [ns]
  (subs (str/replace (str ns) "/" ".") 1))

;;FIXME: only include polymer stuff on demand
(defn config-polymer-defaults
  []
  (doseq [[chan-kw handler] polymer-map]
    (configure-netspace! :get (subs (str chan-kw) 1) handler))
  (mcomm/dump-dispatch-map :get))

(defn config-polymer-reqs
  [reqs]
  (log/trace "config-polymer-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          pm-path (ns-to-path nmsp)
          pm-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " pm-path)
      (configure-netspace! :get nmsp
                               #(do (log/trace "HANDLING POLYMER RQST: " (:uri %))
                                    (let [resp (resource-request % "/")]
                                      (if resp
                                        resp
                                        (do (log/trace "POLYMER RESOURCE NOT FOUND: " (:uri %))
                                            (not-found (:uri %)))))))))
  (mcomm/dump-dispatch-map :get))

(defn config-js-reqs
  [reqs]
  (log/trace "config-js-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          js-path (str nmsp ".*js")] ;;FIXME regex syntax
          ;; js-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " js-path)
      (configure-netspace! :get (symbol js-path)
                               #(do (log/trace "HANDLING JS RQST: " (:uri %))
                                    (let [resp (resource-request % "scripts/")]
                                      (if resp
                                        resp
                                        (do (log/trace "JS NOT FOUND: " (:uri %))
                                            (not-found (:uri %)))))))))
    (mcomm/dump-dispatch-map :get))

(defn config-css-reqs
  [reqs]
  (log/trace "config-css-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          css-path (str nmsp ".*css")] ;;FIXME: regex syntax
          ;; css-chan (chan)]
      (log/trace "reqd ns: " nmsp ", path: " css-path)
      (configure-netspace! :get (symbol css-path)
                     #(do (log/trace "HANDLING CSS RQST: " (:uri %))
                          (let [resp (resource-request % "styles/")]
                            ;; :styles.*  #(resource-request % "/")
                            (if resp
                              resp
                              (do (log/trace "CSS NOT FOUND: " (:uri %))
                                  (not-found (:uri %)))))))))
    (mcomm/dump-dispatch-map :get))

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

;; Match incoming URI+args against netspace and fn formal params
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
  [method netspace-sym behavior]
  (log/trace "configure-netspace! " method netspace-sym (type netspace-sym) behavior (type behavior))
  ;; (doseq [[netspace-sym behavior] disp-map]
  (if (= '* netspace-sym)
    (let [default (if (list? behavior) (eval behavior) behavior)]
      (alter-var-root (var default-observer) (fn [f] default))
      [nil behavior])
    (let [path (if (= netspace-sym '$)
                 "/"
                 (ns-to-path (str netspace-sym)))

          ;; f (cond (list? behavior) (eval behavior) ;;FIXME check for lambda
          ;;         (symbol? behavior) behavior
          ;;         (fn? behavior) behavior
          ;;         :else
          ;;         (throw
          ;;          (Exception.
          ;;           "behavior fn must be symbol, fn, or lambda")))

          dispatch-map (mcomm/get-dispatch-map method)]
      (log/trace "NEW NETSPACE: " method path behavior)
      (swap! dispatch-map
             (fn [old path fn] (assoc old path fn))
             path behavior)
      [path behavior])))
;;      (configure-netspace! path ch co-fn))))

(defn co-fn? [f]
  (if (mcomm/is-lambda? f)
    false
    (if (symbol? f)
      ;; (:co-fn (meta (find-var f)))
      (-> f find-var meta :co-fn)
      (if (var? f)
        ;; (:co-fn (meta f))
        (-> f meta :co-fn)
        false))))

(defn dispatch-rqst
  [rqst behavior]
  (log/trace "dispatch-rqst: " (:uri rqst) behavior (type behavior))
  ;; if fn is co-fn then activate
  (cond
    (co-fn? behavior)
    (do (log/trace "dispatching co-fn " (:uri rqst) (:miraj-baseuri rqst))
        (if (= (:uri rqst) (:miraj-baseuri rqst))
          (do (log/trace "activating co-fn " behavior)
              (let [body (activate behavior)
                    r (xml/serialize :html body)]
                ;; (log/trace "RESULT: " r)
                (response r)))
          (default-observer rqst)))

    (symbol? behavior)
    (do (log/trace "SYMBOL")
        (if (fn? behavior)
          (do (log/trace "FN")
              (let [args (try+ (mcomm/match-params-to-sig rqst behavior)
                               (catch [:type :miraj.http.response/response]
                                   e ;;{:keys [type response]}
                                 (log/trace "CATCH: " e (:type e) (type e))
                                 e))]
                (if (= (:type args) :miraj.http.response/response)
                  (:response args)
                  (do
                    (log/trace "calling ("
                               (-> behavior find-var meta :name) args
                               "), for " (:uri rqst))
                    (if-let [res (apply (deref (-> behavior find-var)) args)]
                      (do (log/trace (-> behavior find-var meta :name) " says: " res)
                          res)
                      (response (not-found))))))) ;;FIXME user-defined not-found
          (do (log/trace "NOT FN: " behavior)
              (response (str (deref (find-var behavior)))))))

    (var? behavior)
    (log/trace "VAR")

    (list? behavior)
    (if (mcomm/is-lambda? behavior)
      (do (log/trace "LAMBDA: " behavior)
          ((eval behavior) rqst))
      (do (log/trace "LIST: " behavior)))

    :else
    (do (log/trace (str "UNKNOWN BEHAVIOR TYPE: " (type behavior) behavior))
        (response (str behavior)))))

  ;; if fn is symbol then match sig and apply if match
  ;; else fn is a lambda, just apply it to rqst

  ;; NB: lambda exprs are the way to get the entire request as a param
  ;; - a lambda expression is a ring handler


(defn start [rqst]
  (log/trace "START HTTP RQST: " (:uri rqst))
    (if-let [dispatch-entry (mcomm/get-dispatch-entry rqst)]
      (do (log/trace "dispatch val: " dispatch-entry)
          (log/trace "dispatch co-fn: " (last dispatch-entry))
          (dispatch-rqst (assoc rqst :miraj-baseuri (first dispatch-entry)) (last dispatch-entry)))
      (default-observer rqst)))

  ;; 2.  match sig - incoming args v. handler's formal params
  ;; 3.  call fn
  ;; NB: this is just like XSLT, there, the path is through the tree.
  ;; (go (>! http-rqst-chan rqst))
  ;; (let [r (<!! http-resp-chan)]
  ;;   ;; (log/trace "responding " r)
  ;;   r))

(log/trace "loaded")
