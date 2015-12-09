(println "starting load miraj.clj")
(ns miraj
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.tools.reader.reader-types :as readers]
            [slingshot.slingshot :refer [try+ throw+]]
            [ring.util.response :as ring :refer [response]]
            [ring.middleware.keyword-params :refer [keyword-params-request]]
            [ring.middleware.params :refer [params-request]]
            [ring.middleware.resource :refer [resource-request]]
            [potemkin.namespaces :refer [import-vars]]
            [miraj.common :as mrj]
            [miraj.sync :as msync]
            ;;FIXME - load async stuff at config time, see below
            ;;[clojure.core.async :as async :refer :all :exclude [into map merge partition reduce take]]
            ;; [miraj.async :as masync]
            [miraj.data.xml :as xml]
            [miraj.html :as h]
            [miraj.http.response :refer [bad-request bad-request! not-found]]
            [ring.util.servlet :as servlet])
  (:import [java.io StringReader StringWriter]
           ;; [javax.servlet.http HttpServlet
           ;;  HttpServletRequest
           ;;  HttpServletResponse]
           [javax.servlet ServletConfig]))

;;  (:require [clojure.pprint :as pp]))

(log/trace "loading")

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

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

;; set html preamble as namespace metadata
(defn set-html-head!
  ;; [docstr reqs & body]
  ;;FIXME: use docstring for <meta name="description"...>
  [nm & args]
  (log/trace "set-html-head! co-ns: " nm)
  (log/trace "set-html-head! this ns: " *ns*)
  (log/trace "set-html-head! args: " (first args))
  (let [ns (create-ns nm)
        ns-path (path-from-ns ns)
        refmap (into {} (map #(identity [(first %) (rest %)]) (rest (first args))))
        ;; log (log/trace "refmap: " refmap)
        title (first (:title refmap))

        polymer-reqs (:polymer refmap)
        log (log/trace "polymer-reqs: " polymer-reqs)

        html-reqs (:html refmap)
        log (log/trace "html-reqs: " html-reqs)

        clj-reqs (:require refmap)
        log (log/trace "clj-reqs: " clj-reqs)

        all-reqs (concat clj-reqs polymer-reqs)
        log (log/trace "ALL REQS: " all-reqs)

        ;; pick out the :requires for co-namespaces create the namespaces
        components (let [comps (filter #(not (some #{:js :css} %)) all-reqs)
                         _ (doseq [c comps]
                             (do (println "CREATE-NS: " (first c))
                                 (create-ns (first c))))]
                     (filter #(do (println "comp: " % " " (find-ns (first %)))
                                  (:co-ns (meta (find-ns (first %)))))
                             comps))
        log (log/trace "components: " components)

        scripts (for [script (filter #(some #{:js} %) html-reqs)] (get-js script))
        log (log/trace "SCRIPTS: " scripts)

        styles  (for [script (filter #(some #{:css} %) html-reqs)] (get-css script))
        log (log/trace "STYLES: " styles)

        links (flatten (for [comp components] (do #_(log/trace "link? " comp) (get-link comp))))
        log (log/trace "LINKS: " links)

        polymer (concat scripts styles links)
        log (log/trace "POLYMER: " polymer)

        preamble (miraj-header title ns-path polymer)
        ;; log (log/trace "PREAMBLE: " preamble)
        ]
    (log/trace (str "*ns*: " ns " " (type ns)))
    (let [_ (println "altering meta on ns: " ns (type ns) (meta ns))
          ;; (alter-meta! n# (fn [m#] (assoc m# :miraj :co-type)))
          newvar (alter-meta! ns
                              (fn [old new] ;;(merge old
                                {:co-ns true :co-fn new})
                              preamble)
          ]
      (log/trace "ns meta: " (meta ns))
      newvar)))

(declare configure-namespace)

(defn get-ns-opts
  [nm refs]
  (log/trace "get-ns-opts " nm)
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
    ;; (if (not (nil? polymer-reqs))
    ;;   (do (log/trace "POLYMER reqs: " polymer-reqs)
    ;;       (config-polymer-reqs polymer-reqs)))

    ;; (if (not (nil? js-reqs))
    ;;   (do (log/trace "JS reqs: " js-reqs)
    ;;       (config-js-reqs js-reqs)))

    ;; (if (not (nil? css-reqs))
    ;;   (do (log/trace "CSS reqs: " css-reqs)
    ;;       (config-css-reqs css-reqs)))

    ;; (log/trace "FOO A")

    ;; (config-polymer-defaults)
    ;; (log/trace "FOO B")
    ;; (mcomm/dump-dispatch-map)
    required))

(defmacro co-ns
  [nm & refs]
  (println "expanding co-ns: " nm " " refs)
  (println "FOO C " *ns*)
  (let [opts (get-ns-opts nm refs)]
    (println "FOO D " opts)
    (eval (macroexpand `(ns ~nm ~@opts)))
    (configure-namespace nm refs)
    (println "FOO E " *ns*)
    `(do (println "invoking co-ns: " '~nm)
       (ns ~nm ~@opts))))

  ;; ;; (eval (apply set-html-head! nm refs))
  ;; `(do (println "invoking co-ns")
  ;;      (let [;;required# (configure-namespace '~nm '~refs)
  ;;            required# (get-ns-args '~nm '~refs)
  ;;            _# (println "FOO C " required#)
  ;;            args# ['~nm required#]
  ;;            _# (println "FOO CC " args#)
  ;;            n# (set-html-head! '~nm '~refs)]
  ;;        (println "FOO D " *ns*)
  ;;        ;; (println "nm: " '~nm (type '~nm))
  ;;        ;; (println "reqs: " '~@required)
  ;;        ;;(configure-namespace '~nm '~refs)
  ;;        `(ns ~'~nm '~required#)
  ;;        (println "FOO E " *ns*)
  ;;        `(ns ~'~nm)
  ;;        (println "FOO F " *ns*)
  ;;        #_(set-html-head! '~nm '~refs))))

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
     ;; (println "COFUNC altering meta for " n# " meta: " (meta n#))
     (alter-meta! n# (fn [m#] (assoc m# :co-fn true)))
     ;; (println "COFUNC altered meta " (meta n#))
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

(defn configure-netspace [method netspace-sym co-fn]
  (throw (Exception. "calling dummy configure-netspace")))

;; (defn config-polymer-defaults []
;;   (throw (Exception. "calling dummy config-polymer-defaults")))

(defmacro >>
  [netspace behavior]
  (log/trace "EXHIBIT-FOR-OBSERVATION, IMMUTABLE: " netspace behavior)
  `(log/trace "EXHIBIT-FOR-OBSERVATION, IMMUTABLE: " '~netspace '~behavior)
  `(configure-netspace :get '~netspace '~behavior))

(defmacro >>!
  [netspace behavior]
  (log/trace "EXHIBIT-FOR-OBSERVATION, MUTABLE: " netspace behavior)
  `(log/trace "EXHIBIT-FOR-OBSERVATION, MUTABLE: " '~netspace '~behavior)
  `(configure-netspace :post '~netspace '~behavior))

(defn start [rqst])

(defn dump-dispatch-map [& method]
  (throw (Exception. "calling dummy dump-dispatch-map")))

(defn config-sync []
  (log/trace "config-sync")
  (alter-var-root
   (var start)                     ; var to alter
   (fn [f]                       ; fn to apply to the var's value
     (fn [rqst]
       (log/trace "SYNC dispatching http rqst: " (:uri rqst))
       ))))

(defn configure-namespace [nm refs]
  (throw (Exception. "calling dummy configure-namespace")))

(defn config-sync []
  (println "config-sync")
  (alter-var-root (var configure-namespace) (fn [f] msync/configure-namespace))
  (alter-var-root (var configure-netspace) (fn [f] msync/configure-netspace!))
  (alter-var-root (var dump-dispatch-map) (fn [f] mrj/dump-dispatch-map))
  (alter-var-root (var start) (fn [f] msync/start)))
;  (intern 'config '-service (servlet/make-service-method msync/start))
;  (println "defservice " (find-var 'config/-service)))
;;  (msync/start-http-observer))

(defn config-async []
  (log/trace "config-async")
  ;;FIXME: load core.async
  ;; (alter-var-root (var configure-namespace) (fn [f] masync/configure-namespace))
  ;; (alter-var-root (var configure-netspace) (fn [f] masync/start-netspace-observer))
  ;; (alter-var-root (var dump-dispatch-map) (fn [f] mrj/dump-dispatch-map))
  ;; (alter-var-root (var start) (fn [f] masync/start))
  #_(masync/start-http-observer))

(defmacro config [mode]
  `(do
     (log/trace "config mode: " ~mode)
     ;;(in-ns 'miraj)
     (condp = ~mode
       :sync (config-sync)
       :async (config-async)
       (throw (Exception. "unrecognized config mode: " ~mode)))))
  ;; (println "expanding config " *ns*)
  ;; (case mode
  ;;   :sync (do
  ;;           (eval (macroexpand '(ns config)))
  ;;           (println "defining service " *ns*)
  ;;           (eval (macroexpand (def config/ -service miraj.sync/start)))
  ;;           (println "defservice " (find-var 'config/-service)))))
  ;;           ;;(eval (macroexpand '(ring.util.servlet/defservice miraj.sync/start)))))


(log/trace "loaded")
