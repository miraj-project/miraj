(ns miraj.core
  (:require [clojure.string :as string]
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
            [ring.util.response :refer [response]])
  (:import [java.io StringReader]))

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

(defonce polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "molecules"})

(defn- get-reqs
  [reqs]
  ;; (log/trace "get-reqs: " reqs)
  (if (not= (first reqs) :require)
    (throw (RuntimeException. (str  "foo"))))
  (let [reqs (seq
               (for [req (next reqs)]
                 (do
                   ;; (log/trace "REQ: " req (type req))
                   (let [pfx (string/split (str (first req)) #"\.")
                         opts (next req)]
                     ;; (log/trace (str "PREFIX: " pfx))
                     (cond
                       (= (first pfx) "polymer")
                       (let [pns (second pfx)
                             refs (:refer (apply hash-map opts))]
                         (if (not (contains? polymer-nss pns))
                             (throw (RuntimeException. (str "unsupported namespace: " (first req)))))
                         (for [ref refs]
                               [:link {:rel "import"
                                       :href (str (first pfx) "/"
                                               (second pfx) "-"
                                               ref "/"
                                               (second pfx) "-"
                                               ref ".html")}]))

                       (keyword? (first opts)) ;; :refer, :js, :css, :html
                       (do #_(log/trace "KEYWORD: " (first opts))
                           (let [args (apply hash-map opts)]
                             ;; (log/trace "args: " args)
                             (if (> (count (keys args)) 1)
                               (str "<h1>ERROR option must be one of :refer, :js, :css or :html</h1>")
                               (cond
                                 (:refer args)
                                 (for [opt (:refer args)]
                                   (do #_(log/trace "require: " opt)
                                       [:link {:rel "import"
                                               :href (str (first pfx) "/"
                                                       (second pfx) "/"
                                                       opt)}]))
                                 (:js args)
                                 [[:script {:src (:js args)}]]
                                 (:css args)
                                 [[:link {:rel "stylesheet" :href (:css args)}]]
                                 (:html args)
                                 [[:link {:rel "import" :href (:html args)}]]
                                 :else (str "<h1>ERROR unsupported option: " (keys args) "</h1>")))))
                       :else
                       (do  (str "<h1>ERROR unsupported option: " opts "</h1>")
                            (log/error "unsupported option: " opts)))))))]
    (mapcat identity reqs)))

  ;; (clojure.core/assert-args
  ;;    (vector? seq-exprs) "a vector for its binding"
  ;;    (even? (count seq-exprs)) "an even number of forms in binding vector")

(defmacro handle-ctor
  [ctor]
  (log/trace "")
  (log/trace "handle-ctor: " ctor)
  ;; (doseq [form ctor]
  ;;   (log/trace "ctor form: " form))
  (if (not (= ('ctor (first ctor))))
    (log/trace "ERROR: first not 'ctor"))
  (if (not (vector? (first (next ctor))))
    (log/trace "ERROR: missing param vector"))
  (if (not (list? (first (nnext ctor))))
    (log/trace "ERROR: missing ctor body"))
  (let [params (first (next ctor))
        body   (first (nnext ctor))]
    (log/trace "params: " params)
    (log/trace "body: " body)
    `(let [body# ~(eval body)]
      (log/trace "body: " body#)
      body#)))
;      (xml/serialize body#))))

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

(defmacro co-type
  [name & args]
  (log/trace "co-type: " name ", nbr args:" (count args))
  ;; (doseq [arg args]
  ;;   (log/trace "arg: " (type arg) ": " arg))
  #_(let [result (for [arg args]
                 (cond
                   (list? arg)
                   (cond
                     (= 'ctor (first arg))
                     (do (log/trace "CTOR")
                         (eval `(handle-ctor ~arg)))
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
                   ))]
      )
  (let [ctor (nth args 2)
        ;;result (eval `(handle-ctor ~ctor))]
        parms (nth ctor 1)
        params (vec (flatten (merge []
                (for [param parms]
                  [param (keyword param)]))))
        body    (list 'let params (first (nnext ctor)))
        ]
    (log/trace "params: " params)
    (log/trace "body: " body)
    `(let [body# ~body]
       (log/trace "new body: " body#)
      body#)))

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

(defmacro co-ns
  ;; {:arglists '([name docstring? attr-map? references*])}
  ;; [docstr reqs & body]
  [arg & args]
  ;; (log/trace "co-ns: " arg)
  (if (symbol? arg)
    (do ;; we're defining co-fns
      ;; (log/trace "co-ns: " arg " - defining co-fns")
      (require arg :reload)
      (if (not (ns-resolve *ns* (symbol "Miraj")))
        (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
      )
    (do ;; we're defining co-routines
      (let [docstr arg
            [reqs body] args
            ns *ns*
            reqs (get-reqs reqs)]
        ;; (log/trace (str "*ns*: " *ns*))
        ;; (log/trace (str "docstr: " docstr))
        ;; (log/trace (str "reqs: " reqs))
        (flush)
        ;; (log/trace "reqs obtained: ")
        ;; (pp/pprint reqs)
        ;; (log/trace "setting up page")
        `(let [;;ns# (create-ns '~ns)
               ;;log# (log/trace "new-ns: " (str ns#))
               var# (intern '~ns (symbol "Miraj"))
               log# (log/trace "var#: " var#)
               val# (alter-var-root
                      var#
                      (fn [oldval#]
                        [:head
                         [:title ~docstr]
                         [:meta {:charset "utf-8"}]
                         [:meta {:name "description" :content ~docstr}]

                         [:meta {:name "viewport",
                                 :content
                                 "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"}]

                         ;; Web Application Manifest
                         [:link {:rel "manifest" :href "manifest.json"}]

                         ;; Chrome for Android theme color
                         [:meta {:name "theme-color" :content "#2E3AA1"}]
                         ;; Add to homescreen for Chrome on Android
                         [:meta {:name "mobile-web-app-capable" :content "yes"}]
                         [:meta {:name "application-name" :content "PSK"}]
                         [:link {:rel "icon" :sizes "192x192"
                                 :href "images/touch/chrome-touch-icon-192x192.png"}]

                         ;; Add to homescreen for Safari on iOS
                         [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
                         [:meta {:name "apple-mobile-web-app-status-bar-style" :content "black"}]
                         [:meta {:name "apple-mobile-web-app-title" :content (str ~docstr)}]
                         [:link {:rel "apple-touch-icon" :href "images/touch/apple-touch-icon.png"}]

                         ;; Tile color for Win8
                         [:meta {:name "msapplication-TileColor" :content "#3372DF"}]
                         ;; Tile icon for Win8 (144x144)
                         [:meta {:name "msapplication-TileImage"
                                 :content "images/touch/ms-touch-icon-144x144-precomposed.png"}]

                         ;; Conventions
                         [:link {:rel "stylesheet"
                                 :href (str "styles/" (ns-to-path ~ns) ".css")}]
                         [:link {:rel "import"
                                 :href (str "themes/" (ns-to-path ~ns) ".html")}]
                         ;; :href "styles/panels-theme.html"}] ;; {{project}}.css
                         [:link {:rel "import" :href "styles/shared/style_modules.html"}]
                         [:style {:is "custom-style" :include "shared-styles"}]
                         [:script {:src "polymer/webcomponentsjs/webcomponents-lite.js"}]
                         ~@reqs
                         ]))]
           var#)))))

(defmacro co-routine
  "define a co-routine. co-routines can only be defined within a
  co-namespace (declared using the 'co-ns' form).  they run in a
  co-application (i.e. webpage).  (Technically, a co-routine is
  represented as an ordinary function with metadata {:miraj :co-routine}.)"
  [name args & body]
  ;; (log/trace "co-routine in ns: " *ns*)
  (if (not (ns-resolve *ns* (symbol "Miraj")))
    (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  `(let [n# (defn ~name ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :miraj :co-routine)))
     ;; (log/trace "co-routine: " (meta n#) n#)
     n#))

(defmacro co-defn
  "define a co-function. i.e. a web component. co-functions can be
  defined in any namespace, but used in co-routines."
  [nm args & body]
  ;; (log/trace (str "co-defn: " (ns-name *ns*) "/" nm " " *ns*))
  `(let [n# (defn ~nm ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :miraj :co-fn)))
     n#))

(defmacro resume
  [coroutine] ;; e.g. foo.bar/main
  ;; [ns main]
  (log/info (str "resume: " coroutine (type coroutine)))
             ;; (namespace coroutine)
             ;; " " (name coroutine)))
  ;; (log/trace (str "resume: ") coroutine (type coroutine))
  (let [ns (symbol (namespace coroutine))
        main (symbol (name coroutine))
        header (symbol (str ns "/Miraj"))
        ;; log (log/trace "resume header:" header)
        co-ns (require ns :reload)
        coroutine (ns-resolve ns main)]
    ;; (log/trace "coroutine meta: " (meta coroutine) coroutine)
    (if (= (:miraj (meta coroutine)) :co-routine)
      (do
        (require ns :reload)
        (html5
          (eval header)                   ; get <meta> from Miraj var
          (apply coroutine nil)))
      (throw (RuntimeException. (str "arg to resume must be defined with
      'miraj.hiccup/co-routine', not 'clojure.core/defn'"))))))

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
