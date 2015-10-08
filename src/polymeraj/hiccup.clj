(ns polymeraj.hiccup
  (:require [clojure.pprint :as pp]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.string :as string]
            [ring.util.response :refer :all]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]))

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
                     ;; (log/trace (str "opts: " opts))
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
                            (log/error "supported option: " opts)))))))]
    (mapcat identity reqs)))

(defmacro co-ns
  ;; {:arglists '([name docstring? attr-map? references*])}
  ;; [docstr reqs & body]
  [arg & args]
  (log/trace "co-ns: " arg)
  (if (symbol? arg)
    (do ;; we're defining co-fns
      (log/trace "co-ns: " arg " - defining co-fns")
      (require arg :reload)

      (if (not (ns-resolve *ns* (symbol "Polymeraj")))
        (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
      )
    (do ;; we're defining co-routines
      (let [docstr arg
            [reqs body] args
            ns *ns*
            reqs (get-reqs reqs)]
        (log/trace (str "*ns*: " *ns*))
        (log/trace (str "docstr: " docstr))
        (log/trace (str "reqs: " reqs))
        (flush)
        ;; (log/trace "reqs obtained: ")
        ;; (pp/pprint reqs)
        ;; (log/trace "setting up page")
        `(let [;;ns# (create-ns '~ns)
               ;;log# (log/trace "new-ns: " (str ns#))
               var# (intern '~ns (symbol "Polymeraj"))
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
                         [:meta {:name "apple-mobile-web-app-title" :content "Polymer Starter Kit"}]
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
  represented as an ordinary function with metadata {:polymeraj :co-routine}.)"
  [name args & body]
  (log/trace "co-routine in ns: " *ns*)
  (if (not (ns-resolve *ns* (symbol "Polymeraj")))
    (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  `(let [n# (defn ~name ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :polymeraj :co-routine)))
     (log/trace "co-routine: " (meta n#) n#)))

(defmacro co-fn
  "define a co-function. i.e. a web component. co-functions can be used in co-routines."
  [name args & body]
  `(let [n# (defn ~name ~args ~@body)]
     (alter-meta! n# (fn [m#] (assoc m# :polymeraj :co-fn)))
     (log/trace "co-fn: " (meta n#) n#)))

(defmacro resume
  ;; FIXME:  [coroutine], e.g. foo.bar/main
  [ns main]
  (log/trace (str "resume: " ns " " main))
  (let [header (symbol (str ns "/Polymeraj"))
        log (log/trace "resume header:" header)
        co-ns (require ns :reload)
        coroutine (ns-resolve ns 'main)]
    (log/trace "coroutine meta: " (meta coroutine) coroutine)
    (if (= (:polymeraj (meta coroutine)) :co-routine)
      (do
        (require ns :reload)
        (html5
          (eval header)                   ; get <meta> from Polymeraj var
          (apply coroutine nil)))
      (throw (RuntimeException. (str "arg to resume must be defined with
      'polymeraj.hiccup/co-routine', not 'clojure.core/defn'"))))))

(defn wrap-component
  [handler ns]
  (fn [request]
    (let [pfx (str "/" (ns-to-path (str ns)))
          u (ns-to-path (:uri request))]
      ;; (log/trace (str "wrap-component request: " u))
      (if (.startsWith u pfx)
        (let [exploded (string/split (subs (:uri request) 1) #"/")
              ns (symbol (string/join "." (drop-last exploded)))
              ns (find-ns ns)
              f (last exploded)
              func (ns-resolve ns (symbol f))
              ]
          ;; (log/trace (str "ns " (pr-str ns)))
          ;; (log/trace (str "f " f))
          ;; (log/trace (str "func " func))
          ;; (pp/pprint (apply func nil))
          (-> (response (html (apply func nil)))
            (content-type "text/html")))
        (handler request)))
    ))

