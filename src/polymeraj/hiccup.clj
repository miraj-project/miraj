(ns polymeraj.hiccup
  (:require [clojure.pprint :as pp]
            [clojure.tools.logging :as log :only [debug info]]
            [clojure.string :as string]
            [ring.util.response :refer :all]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]))

(defmacro body
  [attrs content]
  [:body attrs content])

(defmacro h1
  [args]
  [:h1 args])

(defn- get-reqs
  [reqs]
  (log/trace "get-reqs: " reqs)
  (if (not= (first reqs) :require)
    (throw (RuntimeException. (str  "foo"))))
  (let [reqs (seq
               (for [req (next reqs)]
                 (do
                   (log/trace "REQ: " req (type req))
                   (let [pfx (string/split (str (first req)) #"\.")
                         opts (next req)]
                     ;; (log/trace (str "opts: " opts))
                     (cond
                       (= (first pfx) "polymer")
                       (let [refs (:refer (apply hash-map opts))]
                         ;; FIXME: vet for supported nss: paper, iron, neon, etc.
                         (for [ref refs]
                           [:link {:rel "import"
                                   :href (str (first pfx) "/"
                                           (second pfx) "-"
                                           ref "/"
                                           (second pfx) "-"
                                           ref ".html")}]))

                       (keyword? (first opts))
                       (let [args (apply hash-map opts)]
                         ;; (log/trace "args: " args)
                         (if (> (count (keys args)) 1)
                           (str "<h1>ERROR option must be one of :js or :css or :html</h1>")
                           (cond
                             (:js args)
                             [[:script {:src (:js args)}]]
                             (:css args)
                             [[:link {:rel "stylesheet" :href (:css args)}]]
                             (:html args)
                             [[:link {:rel "import" :href (:html args)}]]
                             :else (str "<h1>ERROR unsupported option: " (keys args) "</h1>"))))

                       :else ;; e.g. [my-app.my-components [foo bar]]
                       (do (log/trace "FALLTHROUGH: " opts)
                         (for [opt (first opts)]
                           [:link {:rel "import"
                                   :href (str (first pfx) "/"
                                           (second pfx) "/"
                                           opt)}]))
                       )))))]
                       ;(throw (RuntimeException. (str "only polymer supported currently")))
    (flush)
    (mapcat identity reqs)))

(defn path-to-ns
  [ns]
  (subs (string/replace (str ns) "/" ".") 1))

(defn ns-to-path
  [ns]
  (string/replace (str ns) #"\.|-" {"." "/" "-" "_"}))

(defn wrap-component
  [handler ns]
  (fn [request]
    (let [pfx (str "/" (ns-to-path (str ns)))
          u (ns-to-path (:uri request))]
      (log/trace (str "wrap-component request: " u))
      (if (.startsWith u pfx)
        (let [exploded (string/split (subs (:uri request) 1) #"/")
              ns (symbol (string/join "." (drop-last exploded)))
              log (log/trace "ns as sym: " ns)
              ns (find-ns ns)
              log (log/trace "ns as ns: " ns)
              f (last exploded)
              func (ns-resolve ns (symbol f))
              ]
          (log/trace (str "ns " (pr-str ns)))
          (log/trace (str "f " f))
          (log/trace (str "func " func))
          ;; (pp/pprint (apply func nil))
          (-> (response (html (apply func nil)))
            (content-type "text/html")))
        (handler request)))
    ))

;; (defn wrap-resource
;;   [handler root-path & [{:keys [loader]}]]
;;   (fn [request]
;;     (or (resource-request request root-path {:loader loader})
;;         (handler request))))

(defmacro co-ns
  ;; {:arglists '([name docstring? attr-map? references*])}
  [docstr reqs & body]
  (log/trace (str "*ns*: " *ns*))
  (log/trace (str "docstr: " docstr))
;;  (log/trace (str "reqs: " reqs))
  (let [ns *ns*
        reqs (get-reqs reqs)]
    (flush)
    ;; (log/trace "reqs obtained: ")
    ;; (pp/pprint reqs)
    (log/trace "setting up page")
    `(let [;;ns# (create-ns '~ns)
           ;;log# (log/trace "new-ns: " (str ns#))
           var# (intern '~ns (symbol "page"))
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
       var#)))

(defmacro resume
  [ns main]
  (log/trace "resume")
  (let [;ns (second ns)
        log (log/trace "resume ns " ns (type ns))
;;        ns-sym (str ns)
        page (symbol (str ns "/page"))
        log (log/trace "resume page:" page)]
    (require (symbol (str ns)) :reload)
    (html5 (eval page) (apply (ns-resolve ns 'main) nil))))



;; (defmacro co-nsx
;;   [ns docstr reqs & body]
;;   (log/trace "co-ns")
;;   `(html5
;;      [:head
;;       [:title ~docstr]
;;       [:meta {:charset "utf-8"}]
;;       [:meta {:name "viewport",
;;               :content
;;               "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"}]
;;       [:meta {:name "mobile-web-app-capable", :content "yes"}]
;;       [:meta {:name "apple-mobile-web-app-capable", :content "yes"}]
;;       [:script {:src "polymer/webcomponentsjs/webcomponents-lite.js"}]
;;       [:link {:rel "import" :href "polymer/iron-flex-layout/classes/iron-flex-layout.html"}]
;;       [:link {:rel "import", :href "polymer/paper-button/paper-button.html"}]
;;       [:link {:rel "import", :href "polymer/paper-material/paper-material.html"}]
;;       ]
;;      [:body {:id (str '~ns)} ~@body]))
