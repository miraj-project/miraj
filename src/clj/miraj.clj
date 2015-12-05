(ns miraj
  (:require [clojure.core.async :as async :refer :all :exclude [into map merge partition reduce take]]
            [clojure.data.xml :as xml]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]
            [slingshot.slingshot :refer [try+ throw+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]])
  (:import [java.io StringReader StringWriter]))

;;  (:require [clojure.pprint :as pp]))

(log/trace "loading")

;; dispatch-map takes URIs to input channels
(defonce dispatch-map (atom {}))
(defonce http-rqst-chan (chan 20))
(defonce http-resp-chan (chan 20))
(defonce css-chan (chan 20))
(defonce js-chan (chan 20))
(defonce default-chan (chan 20))

;;TODO: support config of base path
(def polymer-map
  {:polymer.* #(resource-request % "/")
   :scripts.* #(resource-request % "/")
   :styles.*  #(resource-request % "/")
   :themes.*  #(resource-request % "/")})

(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

  (defn path-to-ns
    [ns]
    (subs (str/replace (str ns) "/" ".") 1))

  (defn ns-to-path
    [ns]
    (let [s (str/replace (str ns) #"\.|-|\*" {"." "/" "-" "_" "*" ".*"})]
      (if (= \/ (first s)) s (str "/" s))))

(defn android-header
  [docstr]
  ;; Chrome for Android theme color
  (list (h/meta {:name "theme-color" :content "#2E3AA1"})
        ;; Add to homescreen for Chrome on Android
        (h/meta {:name "mobile-web-app-capable" :content "yes"})
        (h/meta {:name "application-name" :content "PSK"})
        (h/link {:rel "icon" :sizes "192x192"
                 :href "images/touch/chrome-touch-icon-192x192.png"})))

(defn safari-header
  [docstr]
  ;; Add to homescreen for Safari on iOS
  (list (h/meta {:name "apple-mobile-web-app-capable" :content "yes"})
  (h/meta {:name "apple-mobile-web-app-status-bar-style" :content "black"})
  (h/meta {:name "apple-mobile-web-app-title" :content (str docstr)})
  (h/link {:rel "apple-touch-icon" :href "images/touch/apple-touch-icon.png"})))

(defn win8-header
  [docstr]
  (list   ;; Tile color for Win8
   (h/meta {:name "msapplication-TileColor" :content "#3372DF"})
   ;; Tile icon for Win8 (144x144)
   (h/meta {:name "msapplication-TileImage"
            :content "images/touch/ms-touch-icon-144x144-precomposed.png"})))

(defn polymer-header
  [ns-path]
  [#_(h/link {:rel "stylesheet"
            :href (str "styles/" ns-path "/core.css")})
   (h/link {:rel "import"
            :href (str "themes/" ns-path "/core.html")})
   ;; :href "styles/panels-theme.html"}) ;; {{project}}.css
   (h/link {:rel "import" :href "styles/shared/style_modules.html"})
   (h/style {:is "custom-style" :include "shared-styles"})
   (h/script {:src "polymer/webcomponentsjs/webcomponents-lite.js"})])

(defn meta-header
  [docstr]
  (list (h/title docstr)
   (h/meta {:charset "utf-8"})
   (h/meta {:name "description" :content docstr})
   (h/meta {:name "viewport",
            :content
            "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"})
   ;; Web Application Manifest
   (h/link {:rel "manifest" :href "manifest.json"})))

(defn miraj-header
  [docstr ns-path & reqs]
  (let [hdr (h/head (apply
                     concat (meta-header docstr)
                     (android-header docstr)
                     (safari-header docstr)
                     (win8-header docstr)
                     (polymer-header ns-path)
                     reqs))]
    ;; (log/trace "MIRAJ HEADER: " hdr)
    hdr))

(defn path-from-ns
  [ns]
  (str/replace (str ns) #"\.|-" {"." "/" "-" "_"}))

(defn- get-reqs
  [reqs]
  (log/trace "get-reqs: " reqs)
  ;; (if (not= (first reqs) :require)
  ;;   (throw (RuntimeException. (str  "foo"))))

;; test for component ns:
;;(:co-ns (meta (find-ns 'polymer.paper)))

  (let [reqs (seq
              (for [req reqs]
                (do
                  (log/trace "REQ: " req (type req))
                  (let [pfx (str/split (str (first req)) #"\.")
                        opts (next req)]
                    (log/trace (str "PREFIX: " pfx))
                    (cond
                      (= (first pfx) "polymer")
                      (let [pns (second pfx)
                            refs (:refer (apply hash-map opts))]
                        (if (not (contains? polymer-nss pns))
                          (throw (RuntimeException. (str "unsupported namespace: " (first req)))))
                        (for [ref refs]
                          ;; [:link {:rel "import"
                          (h/link {:rel "import"
                                   :href (str (first pfx) "/"
                                              (second pfx) "-"
                                              ref "/"
                                              (second pfx) "-"
                                              ref ".html")})))

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
                                      (h/link {:rel "import"
                                               :href (str (first pfx) "/"
                                                          (second pfx) "/"
                                                          opt)})))
                                (:js args)
                                [(h/script {:src (:js args)})]
                                (:css args)
                                [(h/link {:rel "stylesheet" :href (:css args)})]
                                (:html args)
                                [(h/link {:rel "import" :href (:html args)})]
                                :else (str "<h1>ERROR unsupported option: " (keys args) "</h1>")))))
                      :else
                      (do  (str "<h1>ERROR unsupported option: " opts "</h1>")
                           (log/error "unsupported option: " opts)))))))]
    (mapcat identity reqs)))

(defn get-href
  [pfx only]
  ;; (log/trace "get-href: " pfx " - " only)
  (let [pfx (str/split (str pfx) #"\.")
        hd (first pfx)]
    ;; (log/trace "get-href pxf: " pfx)
    (cond
      (= hd "polymer")
      (let [pns (second pfx)]
        (if (not (contains? polymer-nss pns))
          (throw (RuntimeException. (str "unsupported namespace: " pns " | " pfx " | " polymer-nss))))
        (if only
          (cond
            (= pns "font") (str hd "/" pns "-" only "/" only ".html")
            :else (str hd "/" pns "-" only "/" pns "-" only ".html"))
          (str hd "/" pns "-elements.html")))
      :else
      (str (str/join "/" pfx) "/" only))))

(defn get-link
 [comp]
 ;; (log/trace (str "get-link: " comp))
 (let [ns (first comp)
       options (apply hash-map (rest comp))
       as-opt (:as options)
       only-opts (:only options)]
   ;; (log/trace "component ns: " ns " :only " only-opts)
   ;; (log/trace "component opts: " options)
   ;; (log/trace "component as: " as-opt)
   (if (nil? only-opts)
     (h/link {:rel "import" :href (get-href ns nil)})
     (for [only only-opts]
       (h/link {:rel "import" :href (get-href ns only)})))))

(defn get-js
  [comp]
  ;; (log/trace "get-js: " comp)
  (if (not= :js (nth comp 1)) (throw (Exception. ":js key must be second item in vector: " comp)))
  (let [ns (first comp)
        options (apply hash-map (nnext comp))
        as-opt (:as options)
        refer-opts (:refer options)]
    ;; (log/trace "component ns: " ns " :refer " refer-opts)
    ;; (log/trace "component opts: " options)
    ;; (log/trace "component as: " as-opt)
    (if (nil? refer-opts)
      (h/script {:type "text/javascript" :src (get-href ns nil)})
      (for [refer refer-opts]
        (h/script {:type "text/javascript" :src (get-href ns (str refer ".js"))})))))

(defn get-css
  [comp]
  ;; (log/trace "get-css " comp)
  (if (not= :css (nth comp 1)) (throw (Exception. ":css key must be second item in vector: " comp)))
  (let [ns (first comp)
        options (apply hash-map (nnext comp))
        as-opt (:as options)
        refer-opts (:refer options)]
    ;; (log/trace "component ns: " ns " :refer " refer-opts)
    ;; (log/trace "component opts: " options)
    ;; (log/trace "component as: " as-opt)
    (if (nil? refer-opts)
      (h/link {:rel "stylesheet" :type "text/css" :href (get-href ns nil)})
      (for [refer refer-opts]
        (h/link {:rel "stylesheet" :type "text/css" :href (get-href ns (str refer ".css"))})))))

(defn handle-refs
  ;; [docstr reqs & body]
  ;;FIXME: use docstring for <meta name="description"...>
  [nm & args]
  (log/trace "handle-refs co-ns: " nm)
  (log/trace "handle-refs this ns: " *ns*)
  (let [ns (create-ns nm)
        ns-path (path-from-ns ns)
        refmap (into {} (map #(identity [(first %) (rest %)]) args))
        ;; log (log/trace "refmap: " refmap)
        title (first (:title refmap))
        reqs (:require refmap)
        ;; log (log/trace "reqs: " reqs)
        ;; pick out the :requires for components only
        components (let [comps (filter #(not (some #{:js :css} %)) reqs)
                         c (doseq [c comps] (require c))]
                     (filter #(do #_(log/trace "comp: " % " " (find-ns (first %)))
                                  (:co-ns (meta (find-ns (first %))))) comps))
        ;; log (log/trace "components: " components)

        scripts (for [script (filter #(some #{:js} %) reqs)] (get-js script))
        ;; log (log/trace "SCRIPTS: " scripts)

        styles  (for [script (filter #(some #{:css} %) reqs)] (get-css script))
        ;; log (log/trace "STYLES: " styles)

        links (flatten (for [comp components] (do #_(log/trace "link? " comp) (get-link comp))))
        ;; log (log/trace "LINKS: " links)

        polymer (concat scripts styles links)
        ;; log (log/trace "POLYMER: " polymer)

        preamble (miraj-header title ns-path polymer)
        ;; log (log/trace "PREAMBLE: " preamble)
        ]
    (log/trace (str "*ns*: " ns " " (type ns)))
    ;; (log/trace "name: " nm (type nm))
    ;; (log/trace "title: " title)
    ;; (log/trace "reqs: " reqs)
    ;; (log/trace "components: " components)
    ;; (log/trace "links: " links)
    ;; (log/trace "preamble: " preamble)
    (let [;ns# (create-ns ~nm)
                                        ;log# (log/trace "new-ns: " (str ns#))
          ;; var# (intern '~ns (symbol "Miraj"))
          log# (println "ns: " ns (type ns))
          ;; (alter-meta! n# (fn [m#] (assoc m# :miraj :co-type)))
          newvar (alter-meta! ns
                              (fn [old new] ;;(merge old
                                {:co-ns true :co-fn new})
                              preamble)
          ]
      newvar)))

  ;; (if (symbol? arg)
  ;;   (do ;; we're defining co-fns
  ;;     ;; (log/trace "co-ns: " arg " - defining co-fns")
  ;;     (require arg :reload)
  ;;     (if (not (ns-resolve *ns* (symbol "Miraj")))
  ;;       (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  ;;     )
    ;; (do ;; we're defining co-routines


(defonce custom-kws #{:title :components})

(declare chan-dispatch)

(defn config-js-reqs
  [reqs]
  (log/trace "config-js-reqs: " reqs)
  (doseq [req reqs]
    (let [nmsp (first req)
          js-path (str (ns-to-path nmsp) ".*\\.js")]
      (log/trace "reqd ns: " nmsp ", path: " js-path)
      (swap! dispatch-map
             (fn [old path ch] (assoc old path ch))
               js-path js-chan)
        (chan-dispatch js-chan  #(do (log/trace "HANDLING JS RQST: " (:uri %))
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
          css-path (str (ns-to-path nmsp) ".*\\.css")]
      (log/trace "reqd ns: " nmsp ", path: " css-path)
      (swap! dispatch-map
             (fn [old path ch] (assoc old path ch))
               css-path css-chan)
      (chan-dispatch css-chan
                     #(do (log/trace "HANDLING CSS RQST: " (:uri %))
                          (let [resp (resource-request % "styles/")]
                               ;; :styles.*  #(resource-request % "/")
                            (if resp
                              resp
                              (do (log/trace "CSS NOT FOUND: " (:uri %))
                                  (not-found (:uri %))))))))))

(defmacro co-ns
  [nm & refs]
  (log/trace "def co-ns: " nm " " refs)
  (eval (apply handle-refs nm refs))
  (let [ref-map (into {} (clojure.core/map
                          #(identity [(first %) (rest %)]) refs))
        requireds (:require ref-map)
        js-reqs (filter #(some #{:js} %) requireds)
        css-reqs (filter #(some #{:css} %) requireds)
        libs (filter #(not (some #{:js :css} %)) requireds)
        required (list (apply list ':require libs))]
    ;; (println "REFS: " refs)
    ;; (println "REQUIRED: " required)
    (if (not (nil? js-reqs))
      (do (log/trace "JS reqs: " js-reqs)
          (config-js-reqs js-reqs)))
    (if (not (nil? css-reqs))
      (do (log/trace "CSS reqs: " css-reqs)
          (config-css-reqs css-reqs)))
    (log/trace "DISPATCH map: " @dispatch-map)
    `(ns ~nm ~@required)))

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
  (log/trace (str "def co-fn: " (ns-name *ns*) "/" nm))
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
        (let [exploded (str/split (subs (:uri request) 1) #"/")
              ;; log (log/trace "EXPLODED: " exploded)
              ;; log (log/trace "ns: " ns (type ns))
              ns (symbol (str/join "." (drop-last exploded)))
              loadns (require ns :reload)
              f (str ns "/" (last exploded))
              ;; log (log/trace "f: " f)
              func (ns-resolve ns (symbol f))
              ]
          ;; (log/trace (str "ns " (pr-str ns)))
          ;; (log/trace (str "f " f))
          ;; (log/trace (str "func " func))
          (let [r (h/html (apply func nil))]
            ;; (log/trace r)
            (response r)))
        (handler request)))
    ))

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


;;    `(xml/serialize :html ~tree))))

(defn get-path-node
  [n path]
  (log/trace "get-path-node: " n path)
  (let [nodes (vec (filter #(not (empty? %)) (str/split path #"/")))
        log (log/trace "nodes: " nodes (count nodes))
        node (if (< n (count nodes))
               (nodes (+ 1 n)) "foo")]
    node))

;;FIXME: validation.  count of plain args must match count of nodes, etc.
;;FIXME: account for base url nodecount
(defn demultiplex
  [arglist rqst]
  (log/trace "demultiplex: " arglist (:uri rqst))
  (let [nodes (vec (filter #(not (empty? %)) (str/split (:uri rqst) #"/")))]
    (log/trace "node count: " (count nodes) ", arg count: " (count (first arglist)))
    (if (not= (- (count nodes) 1) (count (first arglist)))
      (bad-request! (str "Error: url path should have " (count (first arglist)) " nodes."))))
  (let [u (:uri rqst)]
    (let [args (map-indexed
                (fn [i arg]
                  (log/trace "arg " i ": " arg)
                  (if (= \& (first (str arg)))
                    (do (log/trace "found url parm")
                        nil)
                    (let [node (get-path-node i u)]
                      (log/trace "node: " node)
                      node)))
                (first arglist))]
      (log/trace "ARGS: " args)
      args)))

(defn chan-dispatch
  [in-chan behavior]
  (log/trace "chan-dispatch: " in-chan  behavior (type behavior) " fn? " (fn? behavior))
  ;; [chan & behavior]
  ;; creates a pair of gochans
  (let [cofn? (if (symbol? behavior)
                (:co-fn (meta (find-var behavior)))
                (if (var? behavior)
                  (:co-fn (meta behavior))
                  false))

        beh (if (or (fn? behavior) (symbol? behavior)) behavior
                (throw (Exception. "dispatch table entries must be [kw function] pairs")))]
    (log/trace "co-fn? " cofn? behavior)
    (go ;(log/trace "launching netchan " in-chan beh)
         (while true
           (let [rqst# (<! in-chan)
                 uri# (:uri rqst#)]
             ;; (log/trace "resuming netchan: " (str in-chan "->" 'http-resp-chan) " handling: " uri#)
             ;; note: we don't actually do anything with the request
             ;; (log/trace "http-resp chan: " http-resp-chan)
             (if cofn?
               (do (log/trace "activating co-fn " beh (type beh))
                   (let [body (activate beh)
                         r (xml/serialize :html body)]
                     (log/trace "RESULT: " r)
                     (>! http-resp-chan (response r))))
               (do
                 (if (symbol? behavior)
                   (let [v (find-var behavior)
                         m (meta v)
                         args (try+ (demultiplex (:arglists m) rqst#)
                                    (catch [:type :miraj.http.response/response]
                                        {:keys [response]}
                                      (log/trace "caught exc " (:status response))
                                        (>! http-resp-chan response)
                                        nil))]
                     (if (not (nil? args))
                       (do
                         (log/trace "calling named fn " (:name m) ", arity " args ", for " (:uri rqst#))
                         (if-let [res (apply (deref v) args)]
                           (do (log/trace (:name m) " says: " res (count res))
                               (>! http-resp-chan (response res)))
                           (>! http-resp-chan (not-found))))))
                   (do (log/trace "applying lambda " (:uri rqst#) beh)
                       (log/trace "type lambda " (type beh))
                       (log/trace "class lambda " (class beh))
                       (log/trace "meta lambda " (meta beh))
                       (>! http-resp-chan
                           (if-let [res (beh rqst#)]
                             (do ;;(log/trace "if-let " res)
                                 res)
                             (do ;;(log/trace "not found ")
                                 (not-found))))))
                   )))))))

(defn default-dispatch
  [in-chan behavior]
  (log/trace "default-dispatch")
  (go ;(log/trace "launching default chan")
    (while true
      (let [rqst (<! in-chan)
            uri (:uri rqst)]
        (log/trace "default dispatch on: " uri)
        (>! http-resp-chan (behavior rqst))))))

(defn uri->chan
  [uri]
  (log/trace "uri->chan " uri)
  (if-let [res (get @dispatch-map uri)]
    res
    (let [pred (fn [u]
                 (let [re (re-pattern (first u))
                       ;; log (log/trace "re: " re " uri:" uri)
                       res (re-matches re uri)]
                   (if res (log/trace "match: " res " against " re))
                   res))
          to-chan (last (first (filter pred @dispatch-map)))]
      ;; (log/trace "res-chan: " to-chan)
      to-chan)))

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

(defn dispatch
  [disp-map]
  (log/trace "dispatch " disp-map)
  (doseq [[chan-kw handler] disp-map]
    (if (= :* chan-kw)
      (default-dispatch default-chan handler)
      (let [ch (chan)
            path  (ns-to-path (subs (str chan-kw) 1))]
        (log/trace "NEW CHANNEL: " path ": " ch)
        (swap! dispatch-map
               (fn [old p h] (assoc old p h))
               (if (= chan-kw :$) "/" path)
               ch)
        (chan-dispatch ch handler))))
  ;;FIXME: only include polymer stuff on demand
  (doseq [[chan-kw handler] polymer-map]
    (let [ch (chan)
          path  (ns-to-path (subs (str chan-kw) 1))]
      (log/trace "NEW CHANNEL: " path ": " ch)
      (swap! dispatch-map
             (fn [old p h] (assoc old p h))
             (if (= chan-kw :$) "/" path)
             ch)
      (chan-dispatch ch handler)))

  (if (not (disp-map :*))
    (do (log/trace "using default not-found handler")
        (go (while true
              (let [rqst (<! default-chan)
                    uri (:uri rqst)]
                (log/trace "default handler on: " uri)
                (>! http-resp-chan (not-found)))))))

  (log/trace "dispatch-map " (pprint-str @dispatch-map))
  ;; (log/trace "http-rqst chan: " http-rqst-chan)
  ;; (log/trace "http-resp chan: " http-resp-chan)
  ;; (log/trace "default chan: " default-chan)

  (go (while true
         (let [rqst# (<! http-rqst-chan)
               uri# (:uri rqst#)
               log# (log/trace "resuming dispatch for " uri#)
               chan# (if-let [ch (uri->chan uri#)]
                       ch default-chan)]
          ;; (log/trace "DEFAULT CHAN: " default-chan)
          ;; (log/trace "DISPATCHCHAN: " chan#)
          ;; (log/trace "URI: " uri#)
          (>! chan# rqst#)))))

(log/trace "RELOADING CONFIG")

(require 'config :reload)

(log/trace "CONFIGURING DISPATCH TABLE")
(dispatch config/dispatch-table)

(defn start [rqst]
  (log/trace "dispatching http rqst: " (:uri rqst))
  (go (>! http-rqst-chan rqst))
  (let [r (<!! http-resp-chan)]
    ;; (log/trace "responding " r)
    r))

(log/trace "loaded")
