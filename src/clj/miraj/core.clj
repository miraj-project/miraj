(ns miraj.core
  (:require [clojure.core.async :as async :refer :all]
            [clojure.string :as string]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.data.xml :as xml]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            ;; [clojure.tools.analyzer.jvm :as ana.jvm]
            ;; [clojure.tools.analyzer.ast :as ast]
            ;; [clojure.tools.analyzer.passes.jvm.emit-form :as e]
            ;; [cljs.analyzer :as ana]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]
            [ring.util.response :refer [not-found response]]
            [miraj.html :as h])
  (:import [java.io StringReader]))

(log/trace "loading")

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
  (defn emit-str [ast]
    (with-out-str (c/emit ast)))

  ;; A simple helper that takes a stream and returns a lazy sequences of
  ;; read forms.
  (defn forms-seq [stream]
    (let [rdr (readers/indexing-push-back-reader stream 1)
          forms-seq* (fn forms-seq* []
                       (lazy-seq
                        (if-let [form (reader/read rdr nil nil)]
                          (cons form (forms-seq*)))))]
      (forms-seq*)))

  ;; A helper to just read the first s-expression
  (defn read1 [str]
    (first (forms-seq (string-reader str))))

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

  (defn path-to-ns
    [ns]
    (subs (string/replace (str ns) "/" ".") 1))

  (defn ns-to-path
    [ns]
    (string/replace (str ns) #"\.|-" {"." "/" "-" "_"}))


  ;; (clojure.core/assert-args
  ;;    (vector? seq-exprs) "a vector for its binding"
  ;;    (even? (count seq-exprs)) "an even number of forms in binding vector")

  (defmacro handle-params
    [params]
    (log/trace "handle-params: " params)
    (let [parms (vec (flatten (merge []
                                     (for [param params]
                                       [param (keyword param)]))))]
      (log/trace "new params: " parms)
      ;; (let [letparms (list 'let parms)]
      ;;   (log/trace "let params: " letparms)
      parms))

  (defmacro handle-ctor
    [ctor]
    (log/trace "")
    (log/trace "handle-ctor: " ctor)
    ;; (doseq [form ctor]
    ;;   (log/trace "ctor form: " form))
    (let [key (first ctor)
          params (nth ctor 1)
          body (rest (rest ctor))]
      (if (not (= ('ctor key)))
        (log/trace "ERROR: first not 'ctor"))
      (if (not (vector? params))
        (log/trace "ERROR: missing param vector"))
      (if (not (list? body))
        (log/trace "ERROR: missing ctor body"))
      (log/trace "params: " params)
      (log/trace "body: " body)
      (let [forms (for [form body]
                    (do (log/trace "form: " form)
                        (log/trace "formx: " (macroexpand form))
                        (eval form)))]
        (log/trace "FORMS: " (count forms) ": " forms)
        forms)))

  (defmacro co-type
    [name & args]
    (log/trace "co-type: " name ", nbr args:" (count args))
    ;; (doseq [arg args]
    ;;   (log/trace "arg: " (type arg) ": " arg))
    (let [result (for [arg args]
                   (do (log/trace "arg: " arg)
                       (cond
                         (list? arg)
                         (cond
                           (= 'ctor (first arg))
                           (do (log/trace "CTOR")
                               (macroexpand `(handle-ctor ~arg)))
                           ;; (= :require (first arg))
                           ;; (do (log/trace "REQUIRE")
                           ;;     "<require>")
                           ;; :else
                           ;; (do (log/trace "UNKNOWN: " arg)
                           ;;     "<UNKNOWN>")
                           )

                         (string? arg) arg
                         ;; :else
                         ;; (do (log/trace "UNKNOWN TYPE: " arg)
                         ;;       "<UNKNOWN TYPE>")
                         )))
          ctor (nth result 2)]
      ;; (print "RESULT: " ctor)
      `(list ~@ctor)))

  ;; FIXME:  intern as a func in co-ns
  ;; `(let [n# (defn ~nm ~args ~@body)]
  ;;    (alter-meta! n# (fn [m#] (assoc m# :miraj :co-type)))

  ;; (let [ctor (nth args 2)
  ;;       ;;result (eval `(handle-ctor ~ctor))]
  ;;       parms (nth ctor 1)
  ;;       params (vec (flatten (merge [] ;; must be an easier way
  ;;                                   (for [param parms]
  ;;                                     [param (keyword param)]))))
  ;;       id (str name)
  ;;       body1 (list 'h/dom-module {:id id}
  ;;                   (list 'h/template
  ;;                         (first (nnext ctor))))
  ;;       body    (list 'let params body1)
  ;;       ]
  ;;   (log/trace "params: " params)
  ;;   (log/trace "body1: " body1)
  ;;   (log/trace "body: " body)
  ;;   `(let [body# ~body]
  ;;      (log/trace "new body: " body#)
  ;;     body#)))

  ;; (log/trace "result: " result)
  ;; result))

  (defmacro ctor->dom-module
    [ctor]
    ;; eval body to get html string, then emit dom-module
    ;; (log/trace "ctor->template args: " ctor (count ctor))
    (let [nm (first ctor)
          args (second ctor)
          body (nthrest ctor 2)]
      (let [binders (vec
                     (flatten
                      (merge (for [arg args]
                               [(symbol arg)
                                (if (= '% (:tag (meta arg)))
                                  (str "{{" arg "}}")
                                  (str "[[" arg "]]"))]))))]
        ;; (str "{{" arg "}}")]))))]
        (log/trace "ctor->template args: " args)
        (log/trace "ctor->template binders: " binders)
        ;; (log/trace "ctor->template nm: " 'nm)
        (log/trace "ctor->template body: " body (type body) (count body))
        `(let [~@binders]
           (let [;; bod# ~@body
                 mkup# (str "<dom-module id=\""
                            '~nm "\">" \newline
                            "<template>"
                            (apply str ~@body)
                            \newline "</template>"
                            \newline "</dom-module>")]
             ;;         (log/trace "BODY: " bod#)
             mkup#)))))

  (defmacro for-dom
    [binding & body]
    (log/trace "for-dom " binding body)
                                        ;  (log/trace "TOP BINDERS (AUTHOR): " author)
    (let [fst (first binding)]
      (let [[index item] (if (and (vector? fst) (= (first fst) 'index) (= (second fst) 'item))
                           fst
                           [])
            item (if index item fst)
            for-items (second binding)]
        (if (not (= item 'item))
          (throw (RuntimeException. (str "binding form must be 'item' or '[index item]'")))
          (let [binders ['item (if (= '% (:tag (meta item))) "{{item}}" "[[item]]")
                         (symbol for-items) (if (= '% (:tag (meta for-items)))
                                              (str "{{" for-items "}}")
                                              (str "[[" for-items "]]"))]
                binders (if (nil? index) binders
                            (into binders
                                  ['index (if (= '% (:tag (meta index))) "{{index}}" "[[index]]")]))]
            `(let [~@binders]
               (str \newline
                    "<template is=\"dom-repeat\" items=\""
                    ~for-items "\">"
                    ;; {{" '~for-items "}}\">"
                    ~@body
                    \newline
                    "</template>")))))))

  ;; (defmacro dom-repeat
  ;;   [bindings & body]
  ;;   (let [items (second bindings)
  ;;         item (first bindings)
  ;;         thing "item"]
  ;;     `(str "<template is=\"dom-repeat\" items=\"{{"
  ;; ;;       items
  ;;        "}}\">"
  ;;        ~@body
  ;;        "</template>")))

  ;; (defmacro ctor->template
  ;;   [ctorxp]


(defmacro co-routine
  "define a co-routine. co-routines can only be defined within a
  co-namespace (declared using the 'co-ns' form).  they run in a
  co-application (i.e. webpage).  (Technically, a co-routine is
  represented as an ordinary function with metadata {:miraj :co-routine}.)"
  [nm args & body]
  (log/trace "co-routine " nm " in ns: " *ns*)
  (log/trace "co-routine in ns: " *ns*)
  (if (not #_(ns-resolve *ns* (symbol "Miraj"))
           (:co-ns (meta *ns*)))
    (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  `(let [n# (defn ~nm ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :co-routine true)))
     ;; (log/trace "co-routine: " (meta n#) n#)
     n#))

(defmacro co-fn
  "define a co-function. i.e. a web component. co-functions can be
  defined in any namespace, but used in co-routines."
  [nm args & body]
  (log/trace (str "defco-fn: " (ns-name *ns*) "/" nm))
  `(let [n# (defn ~nm ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :co-fn true)))
     n#))

;;FIXME: only "main" allowed for now
(defmacro resume
  [coroutine] ;; e.g. foo.bar/main
  ;; [ns main]
  (log/info (str "resume: " coroutine (type coroutine)))
             ;; (namespace coroutine)
             ;; " " (name coroutine)))
  ;; (log/trace (str "resume: ") coroutine (type coroutine))
  (let [ns (symbol (namespace coroutine))
        ;; main (symbol (name coroutine))
        header (symbol (str ns "/Miraj"))
        ;; log (log/trace "resume header:" header)
        co-ns (require ns :reload)
        coroutine (ns-resolve ns 'main)]
    ;; (log/trace "coroutine meta: " (meta coroutine) coroutine)
    (if (= (:miraj (meta coroutine)) :co-routine)
      (do
        ;; (require ns :reload)
;;FIXME: use xml/serialize
        ;; (html5
        ;;   (eval header)                   ; get <meta> from Miraj var
        ;;   (apply coroutine nil)))
        (xml/serialize :html (h/html header coroutine)))
      (throw (RuntimeException. (str "arg to resume must be defined with
      'miraj.polymer/co-routine', not 'clojure.core/defn'"))))))

(defn wrap-component
  [handler ns]
  (fn [request]
    (let [pfx (str "/" (ns-to-path (str ns)))
          u (ns-to-path (:uri request))]
      ;; (log/info (str "wrap-component request: " u))
      (if (.startsWith u pfx)
        (let [exploded (string/split (subs (:uri request) 1) #"/")
              ;; log (log/trace "EXPLODED: " exploded)
              ;; log (log/trace "ns: " ns (type ns))
              ns (symbol (string/join "." (drop-last exploded)))
              loadns (require ns :reload)
              f (str ns "/" (last exploded))
              ;; log (log/trace "f: " f)
              func (ns-resolve ns (symbol f))
              ]
          ;; (log/trace (str "ns " (pr-str ns)))
          ;; (log/trace (str "f " f))
          ;; (log/trace (str "func " func))
          (let [r (html (apply func nil))]
            ;; (log/trace r)
            (response r)))
        (handler request)))
    ))

(defn get-body
  [ns component]
  (log/trace "get-body ns: " ns)
  (log/trace "get-body: " component (type component))
  ;; (log/trace "get-body class: " (class component) " / " (type component))
    ;; for dev, always reload
  ;; (log/trace "reloading ns " ns)
  ;; (require (ns-name ns) :reload)
  (let [body (if (fn? component) (component)
                 (if (symbol? component) ((resolve component))))]
    (log/trace "body: " body (type body))
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
                log (log/trace "ACTIVAT ns: " ns)
                preamble (if-let [cofn (:co-fn (meta ns))]
                           cofn
                           (throw (RuntimeException.
                                   (str "co-functions must be defined in a co-namespace."))))]
            (log/trace "ACTIVATE PREAMBLE: " preamble)
            (let [body# (get-body ns component)
                  log# (log/trace "ACTIVATE BODY: " body#)
                  tree# (h/html preamble body#)]
              (log/trace "TREE: " tree#)
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
                log# (log/trace "body: " body#)
                tree# (h/html preamble body#)]
            (log/trace "tree: " tree#)
            tree#)))
    :else
    (do (log/trace "activating other: " (type component)))))


;;    `(xml/serialize :html ~tree))))

;; dispatch-map takes URIs to input channels
(defonce dispatch-map (atom {}))

(defonce http-rqst (chan 10))
(defonce http-resp (chan 10))
(defonce default (chan 10))

(defn chan-dispatch
  [in-chan behavior]
  (log/trace "chan-dispatch: " in-chan  behavior (type behavior) " fn? " (fn? behavior))
  ;; [chan & behavior]
  ;; creates a pair of gochans
  (let [;beh (if (list? behavior) (eval behavior) behavior)
        ;;log (log/trace "beh: " beh (type beh) (fn? beh))
        ;; ch (deref (ns-resolve *ns* inchan))
        ;; log (log/trace "ch deref: " ch (type ch))
        ;; in-chan (if (= (type ch) clojure.core.async.impl.channels.ManyToManyChannel)
        ;;           (do (log/trace "FOO") ch)
        ;;           (throw (Exception. "bad netchan sym")))
        ;; log (log/trace "inchan: " in-chan)

        cofn? (if (symbol? behavior)
                (:co-fn (meta (find-var behavior)))
                (if (var? behavior)
                  (:co-fn (meta behavior))
                  false))

        beh (if (or (fn? behavior) (symbol? behavior)) behavior
                (throw (Exception. "dispatch table entries must be [kw function] pairs")))]

        ;; beh (if (list? behavior) (fn [r] (behavior r))
        ;;         (if (symbol? behavior) (find-var behavior)
        ;;             (throw (Exception. "arg must be fn symbol or list"))))]
    (log/trace "co-fn? " cofn? behavior)
    (go (log/trace "launching netchan " in-chan beh)
         (while true
           (let [rqst# (<! in-chan)
                 uri# (:uri rqst#)]
             (log/trace "resuming netchan: " (str in-chan "->" 'http-resp) " handling: " uri#)
             ;; note: we don't actually do anything with the request
             (log/trace "http-resp: " http-resp)
             (>! http-resp
                 (if cofn?
                   (do (log/trace "activating co-fn " beh (type beh))
                       (let [body (activate beh)
                             r (xml/serialize :html body)]
                         (log/trace "RESULT: " r)
                         (response r)))
                   (do (log/trace "calling fn " beh)
                       (if-let [res (beh rqst#)]
                         res
                         (not-found "foo"))))))))))

(defn dispatch
  [disp-map]
  (log/trace "dispatch " disp-map)
  (doseq [[k handler] disp-map]
    (let [ch (chan)]
      (log/trace "NEW CHANNEL: " ch " for " k)
      (if (not= :_DEFAULT k)
        (swap! dispatch-map
               (fn [old k handler] (assoc old k handler))
               (ns-to-path (subs (str k) 1))
               (if (= :_DEFAULT k) default ch)))
      (chan-dispatch (if (= :_DEFAULT k) default ch) handler)))
  (log/trace "dispatch-map " @dispatch-map)
  (log/trace "http-rqst chan: " http-rqst)
  (log/trace "http-resp chan: " http-resp)
  (log/trace "default chan: " default)
  (go (while true
         (let [rqst# (<! http-rqst)
               log# (log/trace "resuming dispatch")
               uri# (:uri rqst#)
               chan# (if-let [ch (get @dispatch-map uri#)]
                       ch default)]
          (log/trace "DEFAULT CHAN: " default)
          (log/trace "DISPATCHCHAN: " chan#)
          (log/trace "URI: " uri#)
          (>! chan# rqst#)))))

(log/trace "BUZ")

(require 'config :reload)

(log/trace "BooZ")

(defn start [rqst]
  (log/trace "dispatching http")
  (go (>! http-rqst rqst))
  (<!! http-resp))
