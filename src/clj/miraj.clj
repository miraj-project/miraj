(ns miraj
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [miraj.html :as h]))
;;  (:require [clojure.pprint :as pp]))

(println "loading miraj")

(defonce polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "molecules"})

(defn android-header
  [docstr]
  ;; Chrome for Android theme color
  [(h/meta {:name "theme-color" :content "#2E3AA1"})
  ;; Add to homescreen for Chrome on Android
  (h/meta {:name "mobile-web-app-capable" :content "yes"})
  (h/meta {:name "application-name" :content "PSK"})
  (h/link {:rel "icon" :sizes "192x192"
           :href "images/touch/chrome-touch-icon-192x192.png"})])

(defn safari-header
  [docstr]
  ;; Add to homescreen for Safari on iOS
  [(h/meta {:name "apple-mobile-web-app-capable" :content "yes"})
  (h/meta {:name "apple-mobile-web-app-status-bar-style" :content "black"})
  (h/meta {:name "apple-mobile-web-app-title" :content (str docstr)})
  (h/link {:rel "apple-touch-icon" :href "images/touch/apple-touch-icon.png"})])

(defn win8-header
  [docstr]
  [   ;; Tile color for Win8
   (h/meta {:name "msapplication-TileColor" :content "#3372DF"})
   ;; Tile icon for Win8 (144x144)
   (h/meta {:name "msapplication-TileImage"
            :content "images/touch/ms-touch-icon-144x144-precomposed.png"})])

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

(defn miraj-header
  [docstr ns-path & reqs]
  (println "REQS: " reqs)
  (let [hdr (h/head
             (h/title docstr)
             (h/meta {:charset "utf-8"})
             (h/meta {:name "description" :content docstr})
             (h/meta {:name "viewport",
                      :content
                      "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"})
             ;; Web Application Manifest
             (h/link {:rel "manifest" :href "manifest.json"})
             (android-header docstr)
             (safari-header docstr)
             (win8-header docstr)
             (polymer-header ns-path)
             (vec reqs))]
    (log/trace "HDR: " hdr)
    hdr))

(defn ns-to-path
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

(defn handle-refs
  ;; {:arglists '([name docstring? attr-map? references*])}
  ;; [docstr reqs & body]
  [& args]
  (log/trace "handle-refs: " args)
  (let [refmap (into {} (map #(identity [(first %) (rest %)]) args))]
    (pp/pprint refmap)
  ;; (if (symbol? arg)
  ;;   (do ;; we're defining co-fns
  ;;     ;; (log/trace "co-ns: " arg " - defining co-fns")
  ;;     (require arg :reload)
  ;;     (if (not (ns-resolve *ns* (symbol "Miraj")))
  ;;       (throw (RuntimeException. (str "co-routines can only be defined within a co-namespace."))))
  ;;     )
    ;; (do ;; we're defining co-routines
      (let [title (first (:title refmap))



            reqs (:require refmap)
            ;; pick out the :requires for components only
            components (filter #(do
                                  #_(println "REQ: " (first %) " - "
                                             (:co-ns (meta (find-ns (first %)))))
                                  (:co-ns (meta (find-ns (first %))))) reqs)

            calls (for [comp components] (h/link {:rel "import" :href (first comp)}))
            ns *ns*
            ns-path (ns-to-path ns)
            ;; reqs (get-reqs reqs)
            ]
        (log/trace (str "*ns*: " *ns*))
        (log/trace "title: " title)
        (log/trace "reqs: " reqs)
        (log/trace "components: " components)
        (log/trace "CALLS: " calls)

        `(let [;;ns# (create-ns '~ns)
               ;;log# (log/trace "new-ns: " (str ns#))
               var# (intern '~ns (symbol "Miraj"))
               log# (log/trace "var#: " var#)
               val# (alter-var-root
                     var#
                     (fn [oldval#]
                       ~(apply miraj-header title ns-path calls)
                       ))]
           var#))))

(defonce custom-kws #{:title :components})

(defmacro co-ns
  [name & refs]
  (println "co-ns refs: " refs)
  (eval (apply handle-refs refs))
  (let [refmap (into {} (map #(identity [(first %) (rest %)]) refs))
        required (list (apply list ':require (:require refmap)))]
    (println "REFS: " refs)
    (println "REQUIRED: " required)
    `(ns ~name ~@required)))


(println "loaded miraj")
