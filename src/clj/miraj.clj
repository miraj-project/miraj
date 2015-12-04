(ns miraj
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [miraj.html :as h]))
;;  (:require [clojure.pprint :as pp]))

(log/trace "loading")

(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

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
  [(h/link {:rel "stylesheet"
            :href (str "styles/" ns-path ".css")})
   (h/link {:rel "import"
            :href (str "themes/" ns-path ".html")})
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
  (println "miraj-header: " reqs)
  (let [hdr (h/head (apply
                     concat (meta-header docstr)
                     (android-header docstr)
                     (safari-header docstr)
                     (win8-header docstr)
                     (polymer-header ns-path)
                     reqs))]
    (log/trace "MIRAJ HEADER: " hdr)
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
          (str hd "/" pns "-elements.html"))))))

(defn get-link
 [comp]
 (log/trace (str "get-link: " comp))
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
  [script]
  (log/trace "get-js: " script)
  (let [u (nth script 2)]
    (h/script {:type "text/javascript" :src u})))

(defn get-style
  [script]
  (log/trace "get-style " script)
  (let [u (nth script 2)]
    (h/link {:rel "stylesheet" :href u})))

(defn handle-refs
  ;; [docstr reqs & body]
;;FIXME: use docstring for <meta name="description"...>
  [nm & args]
  (log/trace "handle-refs: " args)
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

        styles  (for [script (filter #(some #{:css} %) reqs)] (get-style script))
        ;; log (log/trace "STYLES: " styles)

        links (flatten (for [comp components] (do #_(log/trace "link? " comp) (get-link comp))))
        ;; log (log/trace "LINKS: " links)

        polymer (concat scripts styles links)
        ;; log (log/trace "POLYMER: " polymer)

        preamble (miraj-header title ns-path polymer)
        log (log/trace "PREAMBLE: " preamble)
            ]
    ;; (log/trace (str "*ns*: " ns " " (type ns)))
    ;; (log/trace "name: " nm (type nm))
    ;; (log/trace "title: " title)
    ;; (log/trace "reqs: " reqs)
    ;; (log/trace "components: " components)
    ;; (log/trace "links: " links)
    ;; (log/trace "preamble: " preamble)
    `(let [;ns# (create-ns ~nm)
                                        ;log# (log/trace "new-ns: " (str ns#))
           ;; var# (intern '~ns (symbol "Miraj"))
           ;;log# ~(println "ns: " ns)
           ;; (alter-meta! n# (fn [m#] (assoc m# :miraj :co-type)))
           var# (alter-meta! ;; var-root
                 ;; var#
                 ~ns
                 (fn [current# args#]
                   (merge current# {:co-ns true
                                    :co-fn args#}))
                 '~preamble)
           ]
       var#)))

  ;; (if (symbol? arg)
  ;;   (do ;; we're defining co-fns
  ;;     ;; (log/trace "co-ns: " arg " - defining co-fns")
  ;;     (require arg :reload)
  ;;     (if (not (ns-resolve *ns* (symbol "Miraj")))
  ;;       (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  ;;     )
    ;; (do ;; we're defining co-routines


(defonce custom-kws #{:title :components})

(defmacro co-ns
  [nm & refs]
  (log/trace "co-ns: " nm) ;; " " refs)
  (eval (apply handle-refs nm refs))
  (let [refmap (into {} (map #(identity [(first %) (rest %)]) refs))
        requireds (:require refmap)
        libs (filter #(not (some #{:js :css} %)) requireds)
        required (list (apply list ':require libs))]
    ;; (println "REFS: " refs)
    ;; (println "REQUIRED: " required)
    `(ns ~nm ~@required)))

(log/trace "loaded")
