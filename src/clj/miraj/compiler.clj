(in-ns 'miraj.core)

;; (ns ^{:doc "Miraj web compile functions"
;;       :author "Gregg Reynolds"}
;;   miraj.compiler
;;   (:refer-clojure :exclude [compile import refer require])

(clojure.core/require ;; '[clojure.core :refer :all]
                      '[clojure.string                       :as str]
                      '[clojure.pprint                       :as pp]
                      '[clojure.data.json                    :as json]
                      '[clojure.java.shell :refer [sh]]
                      '[clojure.tools.namespace.repl         :as ctnr :refer [refresh set-refresh-dirs]]
                      '[clojure.pprint                       :as pp]
                      '[clojure.java.io                      :as io]
                      '[clj-time.core                        :as t]
                      '[stencil.core                         :as stencil]
                      ;; [boot.core                            :as boot]
                      ;; [boot.pod                          :as pod]
                      ;; [boot.util                         :as util]
                      ;; [mobileink.boot-bowdlerize         :as bow]
                      '[miraj.co-dom                         :as codom]
                      ;; '[miraj.html                           :as html]
                      '[miraj.core                           :refer :all]
                      '[miraj.utils :as utils]
                      ;; [clojure.tools.reader              :as reader]
                      ;; [clojure.tools.reader.reader-types :as readers]
                      ;; [cljs.analyzer                     :as ana]
                      ;; [cljs.compiler                     :as c]
                      ;; [cljs.closure                      :as cc]
                      ;; [cljs.env                          :as env])
                      '[clojure.tools.logging :as log :only [trace debug error warn info]])

;;   (:import [java.io FileNotFoundException StringWriter]))

;; (log/trace "loading miraj/compiler.clj")

(stencil.loader/set-cache (clojure.core.cache/ttl-cache-factory {} :ttl 0))

(def ^:dynamic *debug* false)
(def ^:dynamic *keep* false)
(def ^:dynamic *verbose* false)

(defn- get-webvars
  "Search namespaces for vars"
  [web-var & nss]
  (log/debug "get-webvars: " web-var nss)
  (let [nss (or nss (all-ns))]
    (let [webvars (for [the-ns nss]
                    (do
                      (let [interns-map (ns-interns the-ns)]
                        (filter (fn [entry] (get (meta (last entry)) web-var)) interns-map))))]
      webvars)))

(defn- modularize-ns
  [the-ns]
  (let [module-prefix   (subs (str the-ns) 0 (str/last-index-of the-ns "."))
        module-name (subs (str the-ns) (+ 1(str/last-index-of the-ns ".")))]
    [module-prefix module-name]))


(defn- bowerize
  [bower-pkg uri cache & verbose]
  (println "    BOWERIZE " bower-pkg cache)
  ;; (let [resource (io/file (.getPath cache) uri)]
  (let [resource (io/file cache uri)]
    (if (.exists resource)
      (if *verbose* (println "bowerize: found cached " uri))
      (let [local-bower  (io/as-file "./node_modules/bower/bin/bower")
            global-bower (io/as-file "/usr/local/bin/bower")
            bcmd         (cond (.exists local-bower) (.getPath local-bower)
                               (.exists global-bower) (.getPath global-bower)
                               :else "bower")
            ;; bower-dir (str cache bower-repo)
            ]
        ;; (io/make-parents bower-dir)

        (let [c [bcmd "install" bower-pkg :dir cache]]
          (println "bower cmd: " c)
          (if *verbose* (println (format "bowerize: installing bower pkg:   %s\n" bower-pkg)))
          (apply sh c))))))


;; finds and templates defcomponent vars
;; for deflibrary vars use link-libraries
;; for defcomponents we use a deflibrary listing the nss to search for defcomponents
#_(defn assemble-component-lib-for-ns
  "Assemble a library of webcomponents."
  [component-ns-sym pprint verbose]
  (if *verbose* (log/debug (format "Assembling lib for webcomponents in ns: %s" component-ns-sym)))
;  (ctnr/refresh)
  (clojure.core/require component-ns-sym)
;;  (binding [*ns* *ns*]
    (let [;;all-nss (all-ns)
          ;;all-nss-names (sort (for [n all-nss] (-> n ns-name)))
          ;; _ (doseq [n all-nss-names] (log/debug "NS: " n))
          component-ns (find-ns component-ns-sym)
          ;; _ (log/debug "component NS: " component-ns)
          component-vars (remove nil? ;;(for [component-ns-sym all-nss]
                                        (miraj/get-component-vars-for-ns component-ns))
          ;; _ (log/debug "component vars: " component-vars)
          ;; _ (doseq [v component-vars] (log/debug "cvar: " v))
          var-maps (for [component-var component-vars]
                     (let [m (-> component-var meta)
                           ;;_ (log/debug "COMPVAR META: " m)
                           href (str (ns->path (:ns m)) "/" (sym->path (:name m)) ".html")
                           elt-fn-meta (update-in m [:miraj/miraj]
                                                  (fn [old]
                                                    (let [;oldmiraj (:miraj/miraj old)
                                                          miraj (assoc
                                                                 {:miraj/defn (symbol (:name m))
                                                                  :miraj/fn (symbol (:name m))
                                                                  :miraj/co-fn true
                                                                  :miraj/element true
                                                                  :miraj/lib :miraj/demo
                                                                  :miraj/assets {:miraj/href href}
                                                                  :miraj/help ""}
                                                                 :miraj/html-tag (keyword (-> old :miraj/html-tag)))]
                                                          ;; new (merge {:miraj/miraj old} clean)]
                                                      miraj)))
                           ]
                       (assoc (:miraj/miraj elt-fn-meta)
                              :miraj/doc {:miraj/short (:doc m)})))
          ;; _ (log/debug "var-maps: " var-maps)
          lib-map {:miraj/ns component-ns-sym :miraj/components (into [] var-maps)}
          ]
      ;; (log/debug "lib-map: " lib-map)
      ;; (doseq [m var-maps] (log/debug (format "map: %s" m)))
      (let [content (stencil/render-file
                     "miraj/templates/componentlib.mustache"
                     lib-map)
            path (sym->path component-ns-sym)
            component-out-path (str/join "/" [*compile-path* (str path ".clj")])
            component-out-file (doto (io/file component-out-path) io/make-parents)]
        (log/debug (format "writing assembly to %s" component-out-path))
        (spit component-out-file content))))

(defn get-component-maps-for-ns-sym
  "get config maps for components in ns"
  [component-ns-sym]
  ;; (if *verbose* (log/debug (format "get-component-maps-for-ns-sym: %s" component-ns-sym)))
  ;; (clojure.core/require component-ns-sym)
;;  (binding [*ns* *ns*]
    (let [;;all-nss (all-ns)
          ;;all-nss-names (sort (for [n all-nss] (-> n ns-name)))
          ;; _ (doseq [n all-nss-names] (log/debug "NS: " n))
          component-ns (find-ns component-ns-sym)
          ;; _ (log/debug "component NS: " component-ns)
          component-vars (remove nil? ;;(for [component-ns-sym all-nss]
                                        (get-component-vars-for-ns component-ns))
          ;; _ (log/debug "component vars: " component-vars)
          ;; _ (doseq [v component-vars] (log/debug "cvar: " v))
          var-maps (for [component-var component-vars]
                     ;;(into {}
                     (let [m (-> component-var meta)
                           ;; _ (log/debug "COMPVAR META: " m)
                           href (str (utils/ns->path (:ns m))
                                     "/"
                                     (utils/sym->path (:name m)) ".html")
                           elt-fn-meta (update-in m [:miraj/miraj]
                                                  (fn [old]
                                                    (let [base (utils/sym->path
                                                                (-> old :miraj/assets
                                                                    :miraj/impl-nss))
                                                          miraj (assoc
                                                                 {:miraj/defn (symbol (:name m))
                                                                  :miraj/co-fn true
                                                                  :miraj/element true
                                                                  :miraj/lib :miraj/demo
                                                                  :miraj/assets {:miraj/href
                                                                                 (str
                                                                                  base ".html")
                                                                                 :miraj/scripts
                                                                                 (str
                                                                                  base
                                                                                  ".cljs")}
                                                                  :miraj/help ""}
                                                                 :miraj/html-tag (keyword (-> old :miraj/html-tag)))]
                                                          ;; new (merge {:miraj/miraj old} clean)]
                                                      miraj)))
                           ]
                       (assoc (:miraj/miraj elt-fn-meta)
                              :miraj/doc {:miraj/short (:doc m)})))
          ;; _ (log/debug "var-maps: " var-maps)
          lib-map {:miraj/ns component-ns-sym :miraj/components (into [] var-maps)}
          ]
      var-maps
      ;;(apply (comp vec concat flatten vector) var-maps)
      ;;lib-map
      #_(let [content (stencil/render-file
                     "miraj/templates/componentlib.mustache"
                     lib-map)
            path (sym->path component-ns-sym)
            component-out-path (str/join "/" [*compile-path* (str path ".clj")])
            component-out-file (doto (io/file component-out-path) io/make-parents)]
        (log/debug (format "writing assembly to %s" component-out-path))
        (spit component-out-file content))))

;; obsolete? see link-pages and link-libraries instead?
(defn link-component-cljs
  [deflib-var verbose]
  ;; (log/debug "link-component-cljs deflib var: " deflib-var)
  ;; (log/debug "link-component-cljs deflib requires: " (-> deflib-var deref))
  ;; (if *verbose* (log/info "Generating component cljs: " ))
  (let [component-nss (map first (-> deflib-var deref :miraj/require))
        ;; _ (log/debug "Component nss: " component-nss)
        _ (doseq [component-ns component-nss] (clojure.core/require component-ns)) ;; :reload))
        component-vars (flatten (get-component-vars component-nss))
        ;; _ (log/debug "Component vars: " component-vars)
        edn-requires (map (fn [component-var]
                            [component-var (-> component-var meta :miraj/miraj :miraj/html-tag)])
                          component-vars)
        edn-require-nss (for [edn-require edn-requires]
                          (let [ns (-> edn-require first meta :ns ns-name)
                                html-tag (name (-> edn-require second))]
                            (symbol (str ns "." html-tag))))
        ;; _ (log/debug "edn-require-nss: " edn-require-nss)
        ;; path (ns->path (-> deflib-var meta :ns ns-name))
        path (utils/var->path deflib-var)
        ;;FIXME: support :optimizations, etc.
        edn-content {:require (apply vector edn-require-nss)
                               #_[(symbol (str/join "." [(-> component-var meta :ns)
                                                      (-> component-var meta
                                                          :miraj/miraj :miraj/html-tag)
                                                      ;; (-> component-var meta :name)
                                                      "core"]))]
                     :boot-reload {:port 9001}
                     :compiler-options {:optimizations :none
                                        :asset-path (str "/" path)
                                        :output-dir path
                                        :output-to  (str path ".js")
                                        }}
        ]
    ;; (println "edncontent: " edncontent)

    (let [edn-file (str/join "/" [*compile-path* (str path
                                                      ".cljs.edn")])
          ;; edn-file (str/join "/" [*compile-path* path (str name ".cljs.edn")])
          ]
      (if *verbose* (log/info (format "Emitting %s" edn-file)))
      (io/make-parents edn-file)
      (spit edn-file edn-content))))

(defn get-component-nss-for-lib
  "get the component spaces for a deflibrary form"
  [deflib-var & verbose]
  ;; (log/debug "get-component-nss-for-lib for deflib var: " deflib-var)
  ;; (log/debug "link-component-cljs deflib requires: " (-> deflib-var deref))
  ;; (if *verbose* (log/info "Generating component cljs: " ))
  (let [component-nss (map first (-> deflib-var deref :miraj/require))
        ;; _ (log/debug "Component nss: " component-nss)
        _ (doseq [component-ns component-nss] (clojure.core/require component-ns)) ;; :reload))
        component-vars (flatten (get-component-vars component-nss))
        ;; _ (log/debug "Component vars: " component-vars)
        edn-requires (map (fn [component-var]
                            ;; (log/debug (format "CVAR META %s %s" component-var (-> component-var meta)))
                            [component-var (-> component-var meta :miraj/miraj :miraj/html-tag)])
                          component-vars)
        ;; _ (log/debug (format "EDN-REQUIRES %s" (seq edn-requires)))
        edn-require-nss (for [edn-require edn-requires]
                          (let [ns (-> edn-require first meta :ns ns-name)
                                html-tag (name (-> edn-require second))]
                            (symbol (str ns "." html-tag))))]
    ;; (log/debug "edn-require-nss: " edn-require-nss)
    edn-require-nss))

(defn get-component-nss-for-component-vars
  "get the component spaces for a deflibrary form"
  [component-vars & verbose]
  ;; (if *debug* (log/debug "get-component-nss-for-component-vars: " component-vars))
  (let [edn-requires (map (fn [component-var]
                            [component-var (-> component-var meta :miraj/miraj :miraj/html-tag)])
                          component-vars)
        ;;_ (log/debug (format "EDN-REQUIRES %s" (seq edn-requires)))
        edn-require-nss (for [edn-require edn-requires]
                          (let [ns (-> edn-require first meta :ns ns-name)
                                html-tag (name (-> edn-require second))]
                            (symbol (str ns "." html-tag))))]
    ;;(log/debug "edn-require-nss: " edn-require-nss)
    edn-require-nss))

(defn link-test-libs
  "Link test webcomponent libraries. This generates the .clj file
  containing element fns for components in the component space."
  [nss-syms]
  (log/info "link-test-libs: " nss-syms)
  (doseq [components-sym nss-syms]
    (clojure.core/require components-sym)
    (let [component-ns (find-ns components-sym)
          _ (log/info "component-ns:" component-ns)
          component-ns-sym (-> component-ns ns-name)
          _ (log/debug (format "ns-sym %s" component-ns-sym))
          component-maps (get-component-maps-for-ns-sym component-ns-sym)
          ]
      (log/debug "COMPONENT-MAPS: " component-maps)
      ;;(doseq [component-map component-maps]
      (let [lib-clj-path (str (utils/ns->path component-ns)) ;; "/test")
            _ (log/debug "LIB-CLJ-PATH: " lib-clj-path)
            lib-clj-file (str lib-clj-path ".clj")
            lib-js-file (str lib-clj-path ".js")
            html-loader-file (str "/" lib-clj-path "_import.html")
            lib-nss [component-ns-sym]

            component-defns (stencil/render-file
                             "miraj/templates/webcomponents.mustache"
                             {:miraj/ns component-ns-sym
                              :miraj/codom html-loader-file
                              :miraj/nss lib-nss
                              :miraj/impl-nss {:miraj/assets {:miraj/href nil}}
                              :miraj/exports component-maps})
            component-out-path (str (str/join "/" [*compile-path* lib-clj-file]))
            component-out-file (doto (io/file component-out-path) io/make-parents)

                              ;; FIXME: one html loader for all libs, not one per lib
                              ;; html-loader (stencil/render-file
                              ;;                 "miraj/templates/html-loader.mustache"
                              ;;                 {:libs component-maps
                              ;;                  :base "/"
                              ;;                  :js lib-js-file})
                              ;; html-loader-path (str (str/join "/" [*compile-path* html-loader-file]))
                              ;; html-loader-out-file (doto (io/file html-loader-path) io/make-parents)
                              ]
        (if *verbose* (log/debug (format "Emitting %s" component-out-path)))
        (spit component-out-file component-defns)
                          ;; (log/debug (format "writing component loader to %s" html-loader-path))
                          ;; (spit html-loader-out-file html-loader)
                          ))
                      ;; else
                  ;;     (assoc (deref defcomponent-var) :miraj/ns component-lib-ns-sym)))
                  ;; (comment "process :miraj/styles here"))
                ))

(defn link-libraries
  "Link webcomponent libraries. This generates the .clj file
  containing element fns for components."
  [nss-syms]
  (if *verbose* (log/info "link-component-libs: " nss-syms))
  (doseq [deflibspace-sym nss-syms]
      (clojure.core/require deflibspace-sym)
      (let [deflibspace-ns (find-ns deflibspace-sym)]
        (if *debug* (log/debug "Processing ns:" (-> deflibspace-ns ns-name)
                                #_(-> deflibspace-ns meta :miraj/miraj :miraj/deflibrary)))
        (if (-> deflibspace-ns meta :miraj/miraj :miraj/deflibrary)
          (do
            (log/info (format "NS %s is a deflibrary space" deflibspace-ns))
            (let [interns (ns-interns deflibspace-ns)
                  ;; _ (log/debug "INTERNS: " interns)
                  ;; _ (doseq [i (seq interns)]
                  ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                  deflib-vars (map second
                                   (filter #(-> % second meta :miraj/miraj :miraj/deflibrary)
                                           (seq interns)))]
              (doseq [deflib-var deflib-vars]
                (log/debug "Processing deflibrary: " deflib-var (-> deflib-var meta :miraj/miraj))
                (let [component-lib-ns-sym (str deflibspace-ns
                                                "." (-> deflib-var meta :name))]
                  (if (= :miraj/elements (-> deflib-var meta :miraj/miraj :miraj/deflibrary))
                    (do
                      ;; (link-component-cljs deflib-var *verbose*)
                      (if *verbose* (log/info "Generating component library: " component-lib-ns-sym))
                      (if-let [ns-vectors (-> deflib-var deref :miraj/require)]
                        (let [component-maps (flatten (for [ns-vector ns-vectors]
                                                        (do
                                                          ;;(log/debug "Processing :miraj/require: " ns-vector)
                                                          (clojure.core/require (first ns-vector))
                                                          (get-component-maps-for-ns-sym
                                                           (first ns-vector)))))]
                          ;; (log/debug "COMPONENT-MAPS: " component-maps)
                          ;;(doseq [component-map component-maps]
                          (let [lib-clj-path (utils/sym->path component-lib-ns-sym)
                                ;; _ (log/debug "LIB-CLJ-PATH: " lib-clj-path)
                                lib-clj-file (str lib-clj-path ".clj")
                                lib-js-file (str lib-clj-path ".js")
                                lib-nss (vec (get-component-nss-for-lib deflib-var))
                                html-loader-file (str "/" lib-clj-path "_import.html")

                                component-defns (stencil/render-file
                                                 "miraj/templates/webcomponents.mustache"
                                                 {:miraj/ns component-lib-ns-sym
                                                  :miraj/codom html-loader-file
                                                  :miraj/nss lib-nss
                                                  :miraj/impl-nss {:miraj/assets {:miraj/href nil}}
                                                  :miraj/exports component-maps})
                                component-out-path (str (str/join "/" [*compile-path* lib-clj-file]))
                                component-out-file (doto (io/file component-out-path) io/make-parents)
                                ]
                            (if *verbose* (log/debug (format "Emitting %s" component-out-path)))
                            (spit component-out-file component-defns)
                            ))
                        (do (log/debug (format "this is a 3rd party wrapper" ))
                            (assoc (deref deflib-var) :miraj/ns component-lib-ns-sym))))
                    (comment "process :miraj/styles here"))
                  ))))
          (if *debug* (log/debug "no deflibrary found in ns" (-> deflibspace-ns ns-name)))
          ))))

(defn- has-deps-edn
  [page-sym]
  ;; assert: page-sym is a sym
  (let [page-path (utils/sym->path page-sym)
        path (str page-path "/deps.edn")
        res  (io/resource path)]
    ;; (log/trace "HAS-DEPS-EDN:" page-sym path res)
    res))

(defn get-external-deps-map
  ;; same as get-css-imports, but returns a map instead of elements
  [ns-path]
  ;; (log/trace "GET-EXTERNAL-DEPS-MAP" ns-path)
  (let [path (str ns-path "/deps.edn")
        res  (io/resource path)
        edn (if res (clojure.edn/read-string (slurp res)) nil)]
    ;; (log/trace "DEPS.EDN:" edn)
    (if edn
      (let [js (for [item (:js edn)]
                 (if (string? item)
                   {:src item}
                   {:src (str "/" (str/replace item #"\." "/") ".js")}))

            ;; css (for [item (:css edn)]
            ;;       {:href (str "/" (str/replace item #"\." "/") ".css")})

            css (:css edn)
            ;; _ (log/trace "CSS DEPS:" (:css edn))
            css (get-css-imports-map (filter #(do ;; (log/trace "CSS:" %)
                                                  (and (not (string? %))
                                                       (not (:custom %))))
                                             css))
            ;; _ (log/trace "CSS IMPORTS MAP result:" css)

            inlined-css (get-inlined-css-map (filter #(string? %) (:css edn)))
            ;; _ (log/trace "INLINED CSS MAP result:" inlined-css)

            custom-inlined-css (get-custom-inlined-css-map (filter #(:custom %) (:css edn)))
            ;; _ (log/trace "CUSTOM INLINED CSS MAP result:" custom-inlined-css)

            imports (for [item (:imports edn)]
                      {:href (str "/" (str/replace item #"\." "/") ".html")})
            ;; _ (log/trace "IMPORTS:" imports)

            ;; styles (for [item (:styles edn)]
            ;;          {:href (str "/" (str/replace item #"\." "/") ".html")
            ;;           :module })
            ;; styles [{:href "/styles/demo.html"
            ;;          :modules [{:module "sweetest"}
            ;;                    {:module "foo"}]}]
            polymers (:miraj.polymer/styles edn)
            polymer-styles (apply merge-with concat (for [p polymers]
                                                      (do (log/trace "P:" p)
                                                      (get-polymer-style-map p))))
            ;; _ (log/trace "POLYMER styles:" polymer-styles)

            result (merge css
                          inlined-css
                          custom-inlined-css
                          polymer-styles
                          {:js js
                           :imports imports})]
        ;; (log/trace "MERGED RESULT:" result)
        result)
      nil)))

(defn- write-deps-html
  [page-sym codom-maps]
  ;; (log/trace "FN: write-deps-html" page-sym codom-maps)
  (let [page-ns-sym (if-let [pns (namespace page-sym)] (symbol pns) page-sym)
        pagelib-path (utils/sym->path page-sym)
        pagespace-sym page-sym ;; (-> page-ns ns-name)
        page-ns (find-ns page-ns-sym)
          ;; here (-> v meta :name)
        here (utils/last-seg pagespace-sym)
          ;; _ (log/trace "PAGE VAR:" v)
          ;; must match import link href in html/normalize
          ;; html-loader-file (str here "/deps.html")
        html-loader-file (str/join "/" [pagelib-path "deps.html"])
        ;; _ (log/trace "loader:" html-loader-file)

          ;; a. get external imports from source file <ns>/deps.edn
          external-deps (get-external-deps-map pagelib-path)
        ;; _ (log/trace "EXTERNAL DEPS: " external-deps)

          ;; from normalize: get lexical deps
          ;; first: user-defined component spaces
          miraj-vars (get-miraj-vars-for-pagespace page-ns)
          ;; _ (log/debug "MIRAJ-vars: " miraj-vars)
          miraj-nss (set (map (fn [r] (-> r meta :ns)) miraj-vars))
          ;; _ (log/debug (format "Miraj NSs %s" miraj-nss))

          ;; miraj-built user-defined libs have non-empty :miraj/nss, link import
          ;; href will be constructed from ns
          miraj-libs (set (filter (fn [e]
                                    ;; (log/trace "NS::" (-> e meta))
                                    ;; (log/debug (format "MIRAJ LIB NSS %s %s"
                                    ;;                    e (-> e meta :miraj/miraj :miraj/nss)))
                                    (and
                                     (not (empty? (-> e meta :miraj/miraj :miraj/nss)))
                                     (or (-> e meta :miraj/miraj :miraj/elements)
                                         (-> e meta :miraj/miraj :miraj/styles))
                                     #_(-> e meta :miraj/miraj :miraj/assets :miraj/impl-nss)))
                                  miraj-nss))
          ;; _ (log/debug "MIRAJ LIBS: " miraj-libs)

          vendor-libs (set (filter (fn [e] #_(log/debug (format "MIRAJ LIB NSS %s %s"
                                                                e (-> e meta :miraj/miraj :miraj/nss)))
                                     (and
                                      (empty? (-> e meta :miraj/miraj :miraj/nss))
                                      (or (-> e meta :miraj/miraj :miraj/elements)
                                          (-> e meta :miraj/miraj :miraj/styles))
                                      #_(-> e meta :miraj/miraj :miraj/assets :miraj/impl-nss)))
                                   miraj-nss))
          ;; _ (log/debug "VENDOR LIBS: " vendor-libs)

          vendor-vars (filter (fn [r] (let [rns (-> r meta :ns)] (contains? vendor-libs rns)))
                              miraj-vars)
          ;; _ (doseq [v miraj-vars] (log/debug (format "MIRAJ-VAR %s" v)))
          ;; _ (doseq [v vendor-vars] (log/debug (format "VENDOR-VAR %s" v)))

          lex-deps (into [] (for [miraj-var miraj-vars]
                              {:href (str (-> miraj-var meta :miraj/miraj :miraj/assets :miraj/href))}))
          ;; _ (log/debug (format "LEX DEPS %s" (seq lex-deps)))


          ;; FIXME: one html loader for all libs, not one per lib
          html-loader (stencil/render-file
                       "miraj/templates/html-loader.mustache"
                       (merge external-deps
                              {:pagespace pagelib-path
                               :codoms codom-maps
                               :lexdeps lex-deps}))
          ;; {:libs component-maps
          ;;  :base "/"
          ;;  :js lib-js-file})
          html-loader-path (str (str/join "/" [*compile-path* html-loader-file]))
          html-loader-out-file (doto (io/file html-loader-path) io/make-parents)]
      (log/debug (format "Emitting %s" html-loader-file))
      (spit html-loader-out-file html-loader)))

;; one html loader file per pagespace. all pages defpaged in one
;; pagespace will use the same loader.
(defn link-pages
  "Link pages/components - generate html loader and *.cljs.edn files."
  ;; go thru pages, pulling the required component libs, then generate files
  [nss-syms opts]
  (if *verbose* (log/info "Linking: " nss-syms))
  (doseq [pagespace-sym nss-syms]
    (clojure.core/require (if (namespace pagespace-sym) (symbol (namespace pagespace-sym)) pagespace-sym))
    (let [pagespace-ns (if-let [pns (namespace pagespace-sym)]
                         (find-ns (symbol pns))
                         (find-ns pagespace-sym))]
      ;; (if (-> pagespace-ns meta :miraj/miraj :miraj/defpage)
      ;;   (log/trace (format "FOUND defpage ns: %s" pagespace-ns)))
      ;; (if (-> pagespace-ns meta :miraj/miraj :miraj/pagespace)
      ;;   (log/trace (format "FOUND pagespace ns: %s" pagespace-ns)))
      (if (or (-> pagespace-ns meta :miraj/miraj :miraj/pagespace)
              (-> pagespace-ns meta :miraj/miraj :miraj/defpage))
        (do
          (let [ns-map-nss (set (map #(-> % second meta :ns)
                                (filter #(-> % second meta :miraj/miraj) (ns-map pagespace-ns))))
                ;; _ (log/debug "NS-MAP: " ns-map-nss)
                ns-refers-nss (set (map #(-> % second meta :ns)
                                   (filter #(-> % second meta :miraj/miraj) (ns-refers pagespace-ns))))
                ;; _ (log/debug "NS-REFERS: " ns-refers-nss)
                ns-aliases-nss (set (map #(-> % second)
                                    (filter #(-> % second meta :miraj/miraj) (ns-aliases pagespace-ns))))
                ;; _ (log/debug "NS-ALIASES: " ns-aliases-nss)

                ;; FIXME: use a metadatum to filter out miraj.html?
                component-nss (set (filter #(not= (-> % ns-name) 'miraj.html)
                                           (concat ns-map-nss ns-refers-nss ns-aliases-nss)))
                ;; _ (log/debug "COMPONENT NSS: " component-nss)

                ;; 1. iterate over component-nss, pulling out the :miraj/nss and converting to hrefs
                ;;    NB: :miraj/nss lists the cljs implementation nss for each component lib
                cljs-impl-nss (flatten (remove nil? (for [component-ns component-nss]
                                                 (do #_(log/debug (format "NS %s" component-ns))
                                                     #_(log/debug (format "META: %s"
                                                                        (-> component-ns meta :miraj/miraj :miraj/nss)))
                                                     (let [nss (-> component-ns meta
                                                                   :miraj/miraj :miraj/nss)]
                                                       (if (empty? nss) nil nss))))))
                ;; _ (log/debug (format "CLJS_IMPL_NSS %s" (seq cljs-impl-nss)))

                pagelib-path (utils/sym->path pagespace-sym)
                      ;; _ (log/debug "PAGELIB-PATH: " pagelib-path)

                ;; FIXME: take :base-path into account
                js-path (str pagelib-path "/js")
                ;; _ (log/debug (format "JS-PATH %s" js-path))

                ;; JS MODULES
                ;; FIXME: one module per page, not per component in page!
                ;; :entries #{} must contain all the component nss for the page module
                ;; modules (into {} (merge-with concat (for [impl-ns cljs-impl-nss]
                ;;           (let [[pfx nm] (modularize-ns impl-ns)
                ;;                 module {(keyword nm) {:output-to (str js-path "/" nm ".js")
                ;;                         ;(str (sym->path pfx) "/js")
                ;;                                       :entries
                ;;                                       #{impl-ns}}}] ;;impl-ns
                ;;             (log/debug (format "MODULE %s" module))
                ;;             module))))
                modules {(keyword pagespace-sym) {:output-to js-path
                                                  :entries (into #{} cljs-impl-nss)}}
                ;; _ (log/debug (format "MODULES %s" modules))

                codom-maps (vec (map (fn [ns]
                                       {:loader (str "/" (utils/sym->path ns) ".html")})
                                     cljs-impl-nss))
                ;; _ (log/debug "CODOM-MAPS: " codom-maps)

                interns (ns-interns pagespace-ns)
                ;; _ (log/debug "INTERNS: " interns)
                ;; _ (doseq [i (seq interns)]
                ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                defpage-vars (if (-> pagespace-ns meta :miraj/miraj :miraj/defpage)
                               [pagespace-ns]
                               (map second
                                  (filter #(-> % second meta :miraj/miraj :miraj/defpage)
                                          (seq interns))))
                ;; _ (log/trace "DEFPAGE-VARS:" defpage-vars)

                pagelib-file (str pagelib-path ".clj")
                pagelib-js-file (str pagelib-path ".js")]

            ;; FIXME: only write deps.html file if one of deps.edn and codoms is present

            ;; (log/trace "PAGELIB-PATH: " pagelib-path)
            (if (or (not (empty? codom-maps))
                    (has-deps-edn pagespace-sym))
              (write-deps-html pagespace-sym codom-maps)
              #_(embed-lexdeps))

            (if (not (empty? codom-maps))
              (do
                ;; 2. write cljs.edn file
                (let [;;js-path (str pagelib-path "/js")
                      edn-content {:require (apply vector cljs-impl-nss)
                                   :compiler-options {:optimizations :none
                                                      :asset-path (str "/" js-path)
                                                      :output-dir js-path
                                                      ;; must match html/normalize
                                                      :output-to  (str js-path "/components.js")
                                                      ;; FIXME: boot-cljs broken
                                                      ;;:modules modules
                                                      }}
                      ;; (println "edncontent: " edncontent)
                      edn-file (str/join "/" [*compile-path* (str pagelib-path ".cljs.edn")])
                        ;; edn-file (str/join "/" [*compile-path* path (str name ".cljs.edn")])
                        ]
                    (if *verbose* (log/info (format "Emitting %s" edn-file)))
                    (io/make-parents edn-file)
                    (spit edn-file edn-content))))
            ))
        ))))

;; ????????????????
(defn link-test-pages
  "Link test pages.  We don't have any pagespaces; instead we use the
  component space to generate the html loader and .cljs.edn files."
  [nss-syms]
  (if *verbose* (log/info "link-test-pages: " nss-syms))
  (doseq [ns-sym nss-syms]
    (log/debug (format "Processing ns %s" ns-sym))
    (let [this-ns (if-let [this-ns (find-ns ns-sym)]
                    this-ns
                    (do (clojure.core/require ns-sym)
                        (find-ns ns-sym)))
          ns-map-nss (map #(-> % second meta :ns)
                          (filter #(-> % second meta :miraj/miraj) (ns-map this-ns)))
          ;; _ (log/debug "NS-MAP: " ns-map-nss)
          ns-refers-nss (map #(-> % second meta :ns)
                             (filter #(-> % second meta :miraj/miraj) (ns-refers this-ns)))
          ;; _ (log/debug "NS-REFERS: " ns-refers-nss)
          ns-aliases-nss (map #(-> % second)
                              (filter #(-> % second meta :miraj/miraj) (ns-aliases this-ns)))
          ;; _ (log/debug "NS-ALIASES: " ns-aliases-nss)

          maybe-component-nss (set (concat ns-map-nss ns-refers-nss ns-aliases-nss))
          ;; _ (log/debug "MAYBE COMPONENT NSS: " maybe-component-nss)

          ;; 1. iterate over component-nss, pulling out the :miraj/nss and converting to hrefs
          component-nss (flatten (remove nil? (for [component-ns maybe-component-nss]
                                                (do #_(log/debug (format "NS %s" component-ns))
                                                    #_(log/debug (format "META: %s"
                                                                         (-> component-ns meta :miraj/miraj :miraj/nss)))
                                                    (-> component-ns meta :miraj/miraj :miraj/nss)))))
          ;; component-nss (conj component-nss this-ns)
          _ (log/debug (format "component-nss %s" component-nss))

          this-component-vars (first (get-component-vars #{(-> this-ns ns-name)}))
          _ (log/debug (format "this components %s" (seq this-component-vars)))

          this-component-nss (get-component-nss-for-component-vars this-component-vars)
          _ (log/debug (format "this-component-nss %s" (seq this-component-nss)))

          component-nss (into component-nss this-component-nss)

          codom-maps (vec (map (fn [ns] {:loader (str "/" (utils/sym->path ns) ".html")}) component-nss))
          _ (log/debug "CODOM-MAPS: " codom-maps)

          ;; add helper ns
          component-nss (into component-nss
                              (for [this-var this-component-vars] (utils/var->ns this-var)))
          _ (log/debug (format "components cljs nss %s" component-nss))

          interns (ns-interns this-ns)
          ;; _ (log/debug "INTERNS: " interns)
          ;; _ (doseq [i (seq interns)]
          ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
          defpage-vars (map second
                            (filter #(-> % second meta :miraj/miraj :miraj/defpage)
                                    (seq interns)))
          pagelib-path (utils/sym->path ns-sym)
          ;; _ (log/debug "PAGELIB-PATH: " pagelib-path)
          pagelib-file (str pagelib-path ".clj")
          pagelib-js-file (str pagelib-path ".js")]
      (if (not (empty? codom-maps))
        (do
          ;; 1. write the loader file
          (let [;; must match import link href in html/normalize
                html-loader-file (str/join "/" [pagelib-path "miraj-imports.html"])

                ;; FIXME: one html loader for all libs, not one per lib
                html-loader (stencil/render-file
                             "miraj/templates/html-loader.mustache"
                             {:pagespace pagelib-path
                              :codoms codom-maps})
                ;; {:libs component-maps
                ;;  :base "/"
                ;;  :js lib-js-file})
                html-loader-path (str (str/join "/" [*compile-path* html-loader-file]))
                html-loader-out-file (doto (io/file html-loader-path) io/make-parents)]
            (log/debug (format "Emitting %s" html-loader-path))
            (spit html-loader-out-file html-loader))
          ;; 2. write cljs.edn file
          (let [js-path (str pagelib-path "/js")
                edn-content {:require (apply vector component-nss)
                             :compiler-options {:optimizations :none
                                                :asset-path (str "/" js-path)
                                                :output-dir js-path
                                                ;; must match html/normalize
                                                :output-to  (str js-path "/components.js")
                                                }}
                ;; (println "edncontent: " edncontent)
                edn-file (str/join "/" [*compile-path* (str pagelib-path ".cljs.edn")])
                ;; edn-file (str/join "/" [*compile-path* path (str name ".cljs.edn")])
                ]
            (if *verbose* (log/info (format "Emitting %s" edn-file)))
            (io/make-parents edn-file)
            (spit edn-file edn-content))))
      )))

(defn create-master-demo-page
  "Generate master demo page. Always at /index.html"
  [nss-syms]
  (if *verbose* (log/info "create-master-demo-page: " nss-syms))
  ;;(doseq [ns-sym nss-syms] (log/debug (format "NS META %s %s" ns-sym (find-ns ns-sym))))
  (let [page-links (into []
                         (flatten (mapcat merge
                         (for [ns-sym (filter
                                       (fn [sym]
                                         (or (-> sym find-ns meta :miraj/miraj :miraj/defpage)
                                             (-> sym find-ns meta :miraj/miraj :miraj/pagespace)))
                                       nss-syms)]
                           (do
                             ;; (log/debug (format "PROCESSING ns %s" ns-sym))
                             (let [ns (find-ns ns-sym)
                                   ;; _ (log/debug (format "NS meta %s" (-> ns meta)))
                                   pages (get-page-vars-for-ns ns)]
                               ;; (log/debug (format "pages %s" pages))
                               (map (fn [p] ;; (log/debug (format "page meta %s %s" p (-> p meta)))
                                      {:href (str (utils/var->path p) ".html")
                                       :name (-> p meta :ns ns-name)
                                       :title (-> p meta :miraj/miraj
                                                  :miraj.html/meta
                                                  :miraj.html/title)
                                       :desc (-> p meta :miraj/miraj
                                                 :miraj.html/meta
                                                 :miraj.html/description)}) pages)))))))
        ;; _ (log/debug (format "PAGE-LINKS %s" page-links))

        demopage-ns 'index
        demopage-path "/"
        demopage-filename "index.html"
        ;; demo-pages-vec (reverse (into [] (seq (for [page pages]
        ;;                                         {:page page
        ;;                                          :html-tag (clojure.core/name
        ;;                                                     (-> page meta
        ;;                                                         :miraj/miraj :miraj/html-tag))}))))
        ;;   _ (log/debug (format "demo-pages-vec %s" demo-pages-vec))
        ;;   demos (map #(utils/sym->path %) demo-pages-vec)
          demopage (stencil/render-file
                    "miraj/templates/master-demo-page.html.mustache"
                    {:demos page-links})
          demopage-path (str (str/join "/" [*compile-path* demopage-filename]))
          demopage-out-file (doto (io/file demopage-path) io/make-parents)]
    (if *verbose* (log/debug (format "Emitting %s" demopage-path)))
    (spit demopage-out-file demopage)))

(defn create-lib-test-pages
  "Create one test page showing all components in lib."
  [nss-syms]
  (if *verbose* (log/info "CREATE-LIB-TEST-PAGES: " nss-syms))
  (doseq [deflibspace-sym nss-syms]
      (clojure.core/require deflibspace-sym)
      (let [deflibspace-ns (find-ns deflibspace-sym)]
        (if *debug* (log/debug "Processing ns:" (-> deflibspace-ns ns-name)
                                #_(-> deflibspace-ns meta :miraj/miraj :miraj/deflibrary)))
        (if (-> deflibspace-ns meta :miraj/miraj :miraj/deflibrary)
          (do
            (log/info (format "NS %s is a deflibrary space" deflibspace-ns))
            (let [interns (ns-interns deflibspace-ns)
                  ;; _ (log/debug "INTERNS: " interns)
                  ;; _ (doseq [i (seq interns)]
                  ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                  deflib-vars (map second
                                   (filter #(-> % second meta :miraj/miraj :miraj/deflibrary)
                                           (seq interns)))]
              (doseq [deflib-var deflib-vars]
                (log/debug "Processing deflibrary: " deflib-var (-> deflib-var meta :miraj/miraj))

                ;; for each implementation ns, pull the defcomponent vars

                (let [component-lib-ns-sym (str deflibspace-ns
                                                "." (-> deflib-var meta :name))]
                  (if (= :miraj/elements (-> deflib-var meta :miraj/miraj :miraj/deflibrary))
                    (do
                      ;; (link-component-cljs deflib-var *verbose*)
                      (if *verbose* (log/info "Generating component library: " component-lib-ns-sym))
                      (if-let [ns-vectors (-> deflib-var deref :miraj/require)]
                        (let [component-nss (map first ns-vectors)
                              _ (log/debug (format "COMPONENT NSS %s" component-nss))
                              component-maps (into [] (flatten (for [ns-vector ns-vectors]
                                                        (do
                                                          (clojure.core/require (first ns-vector))
                                                          (get-component-maps-for-ns-sym
                                                           (first ns-vector))))))
                              component-maps (map (fn [m]
                                                    (update-in m [:miraj/html-tag]
                                                        (fn [old] (clojure.core/name old))))
                                                  component-maps)
                              ;; _ (log/debug "COMPONENT-MAPS: " component-maps)

                              component-vars (into [] (flatten (for [ns-vector ns-vectors]
                                                        (do
                                                          (get-component-vars-for-ns
                                                           (first ns-vector))))))
                              ;; _ (doseq [v component-vars]
                              ;;     (log/debug (format "VAR %s %s"
                              ;;                        v (-> v meta
                              ;;                              :miraj/miraj
                              ;;                              :miraj/assets :miraj/impl-nss
                              ;;                              ))))

                              ;; first generate html loader
                              testpage-ns 'index
                              testpage-path "/"
                              base-path (utils/sym->path component-lib-ns-sym)
                              ;; must match import link href in html/normalize
                              html-loader-file (str "/" (str/join "/" [base-path "miraj-imports.html"]))


                              codom-maps
                              (vec
                               (map (fn [v]
                                      {:loader
                                       (str "/"
                                            (utils/sym->path
                                             (-> v meta
                                                 :miraj/miraj :miraj/assets :miraj/impl-nss))
                                            ".html")})
                                    component-vars))
                              _ (log/debug "CODOM-MAPS: " codom-maps)

                              ;; FIXME: one html loader for all libs, not one per lib
                              html-loader (stencil/render-file
                                           "miraj/templates/html-loader.mustache"
                                           {:pagespace base-path
                                            :codoms codom-maps})
                              ;; {:libs component-maps
                              ;;  :base "/"
                              ;;  :js lib-js-file})
                              html-loader-path (str (str/join "/" [*compile-path* html-loader-file]))
                              html-loader-out-file (doto (io/file html-loader-path) io/make-parents)

                              ;; then do .cljs.edn
                              cljs-impl-nss
                              (vec
                               (map (fn [v]
                                      (-> v meta :miraj/miraj :miraj/assets :miraj/impl-nss))
                                    ;;path->ns-sym))
                                    component-vars))
                              _ (log/debug "CLJS-IMPL-NSS: " cljs-impl-nss)


                              js-path (str base-path "/js")
                              edn-content {:require (apply vector cljs-impl-nss)
                                           :compiler-options {:optimizations :none
                                                      :asset-path (str "/" js-path)
                                                      :output-dir js-path
                                                      ;; must match html/normalize
                                                      :output-to  (str js-path "/components.js")
                                                      }}
                              ;; (println "edncontent: " edncontent)
                              edn-file (str/join "/" [*compile-path* (str base-path ".cljs.edn")])

                              ;; then emit test page
                              testpage-filename "index.html"

                              template-data {:ns testpage-ns
                                             :html-import html-loader-file
                                             :js (str "/" base-path "/js/components.js")
                                             :components component-maps ;; component-vec
                                             :component-lib component-lib-ns-sym
                                             }
                              _ (log/debug (format "TEMPLATE data %s" template-data))

                              testpage (stencil/render-file
                                          "miraj/templates/index-page.mustache"
                                          template-data)
                              testpage-path (str (str/join "/"
                                                           [*compile-path* testpage-filename]))
                              testpage-out-file (doto (io/file testpage-path) io/make-parents)]

                          (if *verbose* (log/debug (format "Emitting %s" html-loader-path)))
                          (spit html-loader-out-file html-loader)

                          (if *verbose* (log/info (format "Emitting %s" edn-file)))
                          (io/make-parents edn-file)
                          (spit edn-file edn-content)

                          (if *verbose* (log/debug (format "Emitting %s" testpage-path)))
                          (spit testpage-out-file testpage)))
                        (do (log/debug (format "this is a 3rd party wrapper" ))
                            (assoc (deref deflib-var) :miraj/ns component-lib-ns-sym)))
                    (comment "process :miraj/styles here"))
                  ))))
          (if *debug* (log/debug "no deflibrary found in ns" (-> deflibspace-ns ns-name)))
          ))))

(defn create-test-pages
  "Generate test page for each ns."
  ;;  1.  find deflibs  2. pull defcomponents from deflibs  3. generate testpage
  [nss-syms]
  (if *verbose* (log/info "CREATE-TEST-PAGES: " nss-syms))
  (doseq [ns-sym nss-syms]
    (log/debug (format "Processing ns %s" ns-sym))
    (let [components (first (get-component-vars #{ns-sym}))
          _ (log/debug (format "components %s" components))
          testpage-ns 'index
          testpage-path "/"
          base-path (utils/sym->path ns-sym)
          html-import (str "/" (str/join "/" [base-path "miraj-imports.html"]))
          testpage-filename "index.html" ;;(str "/" (str/join "/" [testpage-path "index.html"]))
          component-vec (reverse (into [] (seq (for [component components] {:component component
                                                     :html-tag (clojure.core/name
                                                                (-> component meta
                                                                   :miraj/miraj :miraj/html-tag))}))))
          _ (log/debug (format "component-vec %s" component-vec))
          testpage (stencil/render-file
                    "miraj/templates/index-page.mustache"
                       {:ns testpage-ns
                        :html-import html-import
                        :js (str "/" base-path "/js/components.js")
                        :components component-vec
                        :component-lib ns-sym})
          testpage-path (str (str/join "/" [*compile-path* testpage-filename]))
          testpage-out-file (doto (io/file testpage-path) io/make-parents)]
      (log/debug (format "Emitting %s" testpage-path))
      (spit testpage-out-file testpage))))

(defn add-polyfill
  [page-ref polyfill]
  ;; (log/debug (format "add-polyfill %s %s" page-ref polyfill))
  ;; (log/debug (format "page meta %s" (-> page-ref meta)))
  (if (not (contains? #{:lite :heavy} polyfill))
    (throw (Exception.
            (format "unrecognized :polyfill param: %s; use :lite or :heavy" polyfill))))
  (alter-meta! page-ref (fn [old new]
                          ;; (log/debug (format "OLD %s" old))
                          (assoc-in old [:miraj/miraj :miraj/polyfill] new))
               polyfill)
  #_(log/debug (format "page meta 2 %s" (-> page-ref meta)))
  )

;; analogous to gcc - call it mcc, miraj compiler collection?
(defn- compile-import
  "Compile web import forms."
   [import imports-config-map assets-out]
  (println "compile-import: " import)
  (let [ns-basic (first import)
        items (next import)]
    (doseq [item items]
      (let [entry (get-in imports-config-map [ns-basic item])]
        (println "IMPORT CONFIG: " entry)
        (if-let [bower (:bower entry)]
          (bowerize (:bower entry) assets-out)
          (if-let [cdn (:cdn entry)]
            (println "CDN: " entry)
            (if-let [f (:file entry)]
              (println "FILE: " entry)
              (throw (Exception. "bad entry in imports.edn: must have :bower, :cdn, or :file " entry)))))
          ))))

(defn compile-polymer-components
  "Compile required web components."
   []
  (println "    COMPILE-POLYMER-COMPONENTS")
  (let [refsets (get-miraj-vars-all-nss)
        _ (log/info "  REFS: " refsets)
        _ (doseq [refs refsets] (doseq [ref refs] (println (first ref) ": "
                                                           (-> (meta (second ref))
                                                               :miraj/miraj :miraj/assets :miraj/bower))))]
    (doseq [refs refsets]
      (doseq [ref refs]
        ;;(println (first ref) ": "
        (bowerize (-> (meta (second ref)) :miraj/miraj :miraj/assets :miraj/bower)
                  (-> (meta (second ref)) :miraj/miraj :miraj/assets :miraj/href)
                  "resources/public"
                  true))))) ;; *verbose*

(defn- compile-polymer-component
  "Compile required web component forms."
   [import imports-config-map cache & verbose]
  (println "    COMPILE-POLYMER-COMPONENT: " import)
  (println "    IMPORTS-config-map: " imports-config-map)
  (let [pns (first import)
        items (apply hash-map (next import))
        segs (str/split (str pns) #"\.")
        seg1 (first segs)
        seg2 (fnext segs)]
    ;; (println "segs: " segs)
    ;; (println "seg1: " seg1)
    ;; (println "seg2: " seg2)
    (if (not= seg2 "polymer") (throw (Exception. "trying to polymer-compile non-polymer component " import)))
    (doseq [pvar (:refer items)]
      ;; (println "PVAR: " pvar)
      (let [v (find-var (symbol (str pns) (str pvar)))
            uri (:uri (meta v))]
        ;; (println "VAR: " v)
        ;; (println "URI: " uri)
          (let [pkg-name (str "PolymerElements/" seg2 "-" pvar)]
            (println "    IMPORT POLYMER: " pkg-name)
            (bowerize pkg-name uri cache *verbose*))))))

(defn- compile-required-component
  "Compile required web component forms."
   [import imports-config-map cache & verbose]
  (println "compile-required-component: " import)
  ;; (println "imports-config-map: " imports-config-map)
  (let [pns (first import)
        items (apply hash-map (next import))
        segs (str/split (str pns) #"\.")
        seg1 (first segs)
        seg2 (fnext segs)]
    ;; (println "segs: " segs)
    ;; (println "seg1: " seg1)
    ;; (println "seg2: " seg2)
    (doseq [pvar (:refer items)]
      ;; (println "PVAR: " pvar)
      (let [v (find-var (symbol (str pns) (str pvar)))
            uri (:uri (meta v))]
        ;; (println "VAR: " v)
        ;; (println "URI: " uri)
        (if (= seg1 "polymer")
          (let [pkg-name (str "PolymerElements/" seg2 "-" pvar)]
            ;; (println "IMPORT POLYMER: " pkg-name)
            (bowerize pkg-name uri cache))
          (println "NONPOLYMER: " pns pvar))))))

(defn- compile-web-resources
  "Process all web resource dependencies in the project."
  [assets-out & verbose]
  (println "compile-web-resources")
  (let [pages (apply hash-map (flatten (remove empty? (apply get-webvars :_page nil))))
        imports-config-map (get-imports-config-map)]
    (println "PAGES: " pages)
    (doseq [[psym pvar] pages]
      (let [page-meta (meta pvar)]
        (doseq [import (:_webimports page-meta)]
          (compile-import import imports-config-map assets-out))
        (doseq [component (remove keyword? (:_webcomponents page-meta))]

          (let [pns (first component)
                items (apply hash-map (next component))
                segs (str/split (str pns) #"\.")
                seg1 (first segs)]
            ;; FIXME: check the metadata, not the ns structure
            (if (and (= seg1 "miraj") (= "polymer" (second segs)))
              (compile-polymer-component component imports-config-map assets-out *verbose*)
              (compile-required-component component imports-config-map assets-out *verbose*))))))))

(defn compile-webcomponent-var-html
  "Compile defcomponent var to html and write to files."
  [component-var pprint verbose]
  ;; (if *verbose* (log/debug "compile-webcomponent-var-html: " component-var))
  (let [path (utils/ns->path (-> component-var meta :ns))
        ;; _ (println "PATH: " path)
        name (str/replace (-> component-var meta :name) #"-" "_")
        codom (-> component-var meta :miraj/miraj :miraj/codom)
        href (str (utils/sym->path (-> component-var meta :miraj/miraj :miraj/assets :miraj/impl-nss))
                  ".html")
        out-file (str/join "/" [*compile-path* href])]
        ;; out-file (str/join "/" [*compile-path* path (str (-> component-var meta :name) ".html")])]
    (io/make-parents out-file)
    (if *verbose* (log/info (format "Emitting %s" out-file)))
    (spit out-file (with-out-str (if miraj.co-dom/*pprint* (codom/pprint codom) (codom/serialize codom))))))
#_(defn compile-webcomponent-var-cljs
  "Compile defcomponent var to cljs and write to files."
  [component-var pprint verbose]
  (println "compile-webcomponent-var-cljs: " component-var)
  (let [path (ns->path (-> component-var meta :ns))
        ;; _ (println "PATH: " path)
        name (str/replace (-> component-var meta :name) #"-" "_")
        codom (-> component-var meta :miraj/miraj :miraj/codom)
        href (-> component-var meta :miraj/miraj :miraj/assets :miraj/href)
        cljs-file (str/join "/" [*compile-path* (utils/sym->path href) ".cljs"])
        ;; cljs-file (str/join "/" [*compile-path* path name "core.cljs"])
        edn-file (str/join "/" [*compile-path* (str href ".cljs.edn")])
        ;; edn-file (str/join "/" [*compile-path* path (str name ".cljs.edn")])
        edn-content {:require [(symbol (str/join "." [(-> component-var meta :ns)
                                                      (-> component-var meta
                                                          :miraj/miraj :miraj/html-tag)
                                                      ;; (-> component-var meta :name)
                                                      "core"]))]
                    :init-fns []
                    :compiler-options {:optimizations :none
                                       :asset-path (str "/" path)
                                       :output-dir path
                                       :output-to  (str (utils/sym->path href) ".js")
                                       ;; :output-to  (str path "/" name ".js")
                                       }}] ;;FIXME: support :optimizations, etc.
    ;; (println "edncontent: " edncontent)
    (if *verbose* (log/info (format "Emitting %s" cljs-file)))
    (io/make-parents cljs-file)
    (io/make-parents edn-file)
    (spit edn-file edn-content)
    (spit cljs-file (-> component-var meta :miraj/miraj :miraj/prototype))))

(defn compile-webcomponents-cljs
  "Compile defcomponent vars to cljs and write to files.
  This does *not* create the .edn.cljs that boot-cljs needs. That is the job of the link fn."
  [nss pprint verbose]
  (log/debug "compile-webcomponents-cljs " nss)
  ;;(ctnr/refresh)
  (let [component-vars (flatten (get-component-vars nss))
        ;; _ (log/debug "Component vars: " component-vars)
        component-nss (set (map (fn [v] (-> v meta :ns)) component-vars))
        ;; _ (log/debug "Component nss: " component-nss)
        ]
    (doseq [component-var component-vars]
      (let [path (utils/ns->path (-> component-var meta :ns))
            ;; _ (log/debug (format "CVAR META %s" (-> component-var meta :miraj/miraj)))
            href (utils/sym->path (-> component-var meta :miraj/miraj :miraj/assets :miraj/impl-nss))
            cljs-file (str *compile-path* "/" href ".cljs")]
        (if *verbose* (log/info (format "Emitting %s" cljs-file)))
        (io/make-parents cljs-file)
        (spit cljs-file (-> component-var meta :miraj/miraj :miraj/prototype))))))

(defn compile-webcomponents-all
  "Process all webcomponents in the project."
  [html-out cljs-out & nss]
  (println "compile-webcomponenents, html-out: " html-out "; cljs-out: " cljs-out ", nss: " nss)
  (let [components (apply hash-map (flatten (remove empty?
                                                    (apply get-webvars :_webcomponent nss))))]
    ;;(println "webcomponents: " components)
    (doseq [c components]
      (compile-webcomponent-var-html (first c) (last c) html-out cljs-out))))

(defn compile-webcomponents-html
  "Compile all webcomponents in namespaces to html."
  [component-nss pprint verbose]
  (if *verbose* (log/debug (format "compile-webcomponents-html %s" component-nss)))
  (doseq [component-ns component-nss]
    (clojure.core/require component-ns)
    (let [component-ns (find-ns component-ns)
          component-vars (get-component-vars-for-ns component-ns)]
      (doseq [component-var component-vars]
        (compile-webcomponent-var-html component-var miraj.co-dom/*pprint* *verbose*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;        PAGE COMPILE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compile-page-nss
  "Process all page vars in list of namespaces."
  [page-nss-syms opts]
  (log/info "VERBOSE?" *verbose*)
  (if *verbose* (log/info (format "RUNNING fn compile-page-nss for %s" (seq page-nss-syms))))
  (doseq [page-ns-sym (seq page-nss-syms)]
    (log/debug "Processing pagespace:" page-ns-sym)
    (clojure.core/require [page-ns-sym])
    (let [page-ns (if-let [pns (find-ns page-ns-sym)]
                    pns)
          _ (log/debug "Page NS: " page-ns)
          page-vars (vals (into {}
                                (let [interns-map (ns-interns page-ns)]
                                  (filter (fn [entry]
                                            (-> entry second meta :miraj/miraj :miraj/defpage))
                                          interns-map))))
          page-refs (vector (if (-> page-ns meta :miraj/miraj :miraj/defpage) page-ns))]

      (log/debug "page vars: " page-vars)
      (log/debug "page ns meta: " (-> page-ns meta :miraj/miraj :miraj/defpage))
      (log/trace "page refs:" page-refs)

      (doseq [page-ref page-refs]
        (log/debug (format "PAGE REF %s" page-ref (-> page-ref meta :ns)))
        ;; (log/debug (format "PAGE deref %s" (deref page-ref)))
        ;; (log/debug (format "PAGE VAR META %s" (-> page-ref meta)))
        (if (:polyfill opts) (add-polyfill page-ref (:polyfill opts)))
        (let [page-name (if (var? page-ref) (-> page-ref meta :name) (utils/last-seg (ns-name page-ref)))
              page-ns (if (var? page-ref) (-> page-ref meta :ns) page-ref)
              base-path (-> page-ref meta :miraj/miraj :miraj/base-path)
              _ (log/debug (format "BASE path %s" base-path))
              _ (log/trace "NS:" (-> page-ref ns-name)) ;; :ns ns-name))
              path (if base-path
                     (str/join "/" [base-path page-name])
                     (utils/ns->path (-> page-ref ns-name)))
              hfile (str/join "/" [*compile-path* (str path ".html")])
              ;; cfile (str/join "/" [cljs-out path (str page-name ".cljs")])
              ]
          (io/make-parents hfile)
          (binding [*ns* page-ns]
            ;;(if *verbose* (log/info (format "Compiling page %s" page-ref)))
            (if *verbose* (log/info (format "Compiling %s to %s" page-ref hfile)))
            ;;(let [out-str (if miraj.co-dom/*pprint*
            (let [out-str (with-out-str (-> page-ref normalize optimize codom/serialize print))]
                            ;; (with-out-str (-> page-ref normalize optimize codom/pprint))
                            ;; (with-out-str (-> page-ref
                            ;;                   normalize optimize codom/serialize print)))]
              (if *verbose* (log/debug (format "Emitting %s" hfile)))
              (spit hfile out-str))))))))

(defn compile-page-ref
  "Compile pageref to html+js and write to *compile-path*."
  [page-ref]
  ;; (if *verbose* (log/debug "COMPILE-PAGE-REF: " page-ref (var? page-ref)))
  ;; (if *verbose* (log/debug "COMPILE-PAGE-REF meta: " (-> page-ref meta)))

  ;; inject polyfill, so imports will work
  ;; inject link import page/imports.html, unless compile-only is specified

  ;; 3 possibilities for page-ref: ns with defpage, ns containing defpages, and defpage var


   (let [[page-ref-name page-ref-ns]
         (if (var? page-ref)
           [(str (-> page-ref meta :name)) (-> page-ref meta :ns)]
           [(str (-> page-ref ns-name)) page-ref])
         html-path (utils/ns->path page-ref-ns)
         page-html-name (if (var? page-ref)
                          (str html-path "/" page-ref-name ".html")
                          (str html-path ".html"))
         hfile (str/join "/" [*compile-path* page-html-name])]
     (io/make-parents hfile)
     (binding [*ns* page-ref-ns]
       (if *verbose* (log/info (format "Compiling %s to %s" page-ref page-html-name)))
       (let [out-str (with-out-str (-> page-ref normalize optimize codom/serialize print))]
         ;; (if *verbose* (log/info (format "Emitting %s" page-html-name)))
         (spit hfile out-str)))))

(defn mcc   ;;  compile
  [{:keys [pages pagespaces
           optimizations compile-only
           imports polyfill
           debug]
    :as opts}]
  ;; (log/trace  (format "mcc %s" opts))
  ;; (let [pagespaces (or pagespaces (if (and page (nil? pages)) nil :all))
  ;;       pages (or pages (if (and page (nil? pagespaces)) nil :all))]
  (if pages
    (doseq [page pages]
      ;; (log/trace "Compiling Page:" page (-> page type))
      ;; we need to reload for repl development - FIXME: put this in the boot task

      ;; (clojure.core/require (if (var? page)
      ;;                         (-> page meta :ns)
      ;;                         page)) ;; :reload-all)

      ;; FIXME: what if page is a var?
      (let [[page-ref page-ns page-var page-sym]
            (cond (var? page) [page (-> page meta :ns) page nil]

                  (symbol? page) (if (namespace page)
                                   ;; must be a var sym
                                   (do
                                     (clojure.core/require [(symbol (namespace page))])
                                     (let [pvar (clojure.core/resolve page)
                                           pns (-> pvar meta :ns)]
                                       [pvar pns pvar page]))
                                     ;; must be an ns sym
                                   (do
                                     ;; for interactive dev we need to reload the ns
                                     ;; should this be the responsibility of the dev?
                                     ;; no, the boot task should do this?
                                     (clojure.core/require page :reload)
                                     (let [pns (find-ns page)]
                                       [pns pns nil page])))

                  (instance? clojure.lang.Namespace page) (do [page page nil (-> page ns-name)]))]
        ;; (log/trace "Page Ref: " page-ref)
        ;; (log/trace "Page ns: " page-ns)
        ;; (log/trace "Page ns meta: " (meta page-ns))
        ;; (log/trace "Page var: " page-var)
        ;; (log/trace "Page var meta: " (meta page-var))
        ;; (log/trace "Page sym: " page-sym)
        ;; (log/trace "Page meta: " (-> page-sym meta))
        (if (and (nil? page-ref) (nil? page-var))
          (throw (Exception. (format "Page not found: %s" page))))
        ;; reloading page is client responsibility - boot task or repl
        ;; (clojure.core/require (-> page-ns ns-name) :reload)
        (if polyfill (add-polyfill page-ref polyfill))
        ;; FIXME: if page has lexical deps OR deps.edn is provided
        ;; FIXME: why not do this in defpage?
        ;; (log/trace "page-sym has deps.edn?" page-sym (has-deps-edn page-sym))
        (if (has-deps-edn page-sym)
          (alter-meta! page-ref (fn [old new]
                                  (assoc-in old [:miraj/miraj :miraj/deps] new))
                       [(str "/" (utils/sym->path page-sym) "/deps.html")]))
        ;; (log/info (format "Compiling page ref %s" page-ref))
        ;; (log/trace "page meta:" (-> page-ref meta))
        ;; (log/trace "page content:" (deref page-ref))
        (compile-page-ref page-ref))))
  (if pagespaces
    (doseq [pagespace-sym pagespaces]
      (log/trace (format "Compiling pagespace %s" pagespace-sym))
      (clojure.core/require [pagespace-sym]) ;; :reload)
      ;; first compile all defpage vars in ns
      (let [pages (get-page-vars-for-ns pagespace-sym)]
        (doseq [page pages]
          (log/trace (format "Compiling pagevar %s" page))
          ;; compile page
          ))
      ;; then compile the ns, if there is a lambda defpage
      (if (-> pagespace-sym find-ns meta :miraj/miraj :miraj/defpage)
        (let [ps (-> pagespace-sym find-ns)]
          (if polyfill (add-polyfill ps polyfill))
          ;; FIXME: if page has lexical deps OR deps.edn is provided
          ;; FIXME: why not do this in defpage?
          ;; (log/trace "pagespace-sym has deps.edn?" pagespace-sym (has-deps-edn pagespace-sym))
          (if (has-deps-edn pagespace-sym)
            (alter-meta! ps (fn [old new]
                              (assoc-in old [:miraj/miraj :miraj/deps] new))
                         [(str "/" (utils/sym->path pagespace-sym) "/deps.html")]))
          (log/trace (format "Compiling lambda-page %s" pagespace-sym))
          (compile-page-ref ps))))))

(defn webdeps
  "Download and cache web resource dependencies."
  [cache & verbose]
  (println "webdeps ")
  (binding [*ns* *ns*]
    ;;(println "REFRESHING")
    ;; (let [e (ctnr/refresh)]
    ;;   (if (= e :ok)
    ;;     (println "refresh result: " e "\n")
    ;;     (do ;; (clojure.repl/pst)
    ;;       (println "Exception " (.getMessage e))
    ;;         (throw e)))) ;;(Exception. "foo")))))
    (compile-web-resources cache *verbose*)))

#_(defn compile
  "Compile clj+cljs to html+js. Optional args: html-out (string),
  cljs-out (string) nss (vector of ns symbols). With no args, process
  all namespaces and write output to './'.  Each namespace will be
  searched for defcomponent and defpage forms, which will be processed
  to generate HTML and Javascript files.

  Also write bowdlerize.edn containing bower configs."
  [page-var]
  #_[& {:keys [html-out cljs-out assets-out nss]
      :or   {html-out "./"
             cljs-out   "./"
             assets-out "./"
             nss nil}}]
  ;; (println "compiler " *ns*)
  (binding [*ns* *ns*]
    ;; (println "REFRESHING")
    (let [e (ctnr/refresh)]
      (if (= e :ok)
        (println "refresh result: " e "\n")
        (do ;; (clojure.repl/pst)
          (println "Exception " (.getMessage e))
            (throw e)))) ;;(Exception. "foo")))))
    #_(compile-webcomponents html-out cljs-out)
    ;; (compile-page-nss html-out cljs-out)
    (compile-page-var page-var)
    #_(get-imports-config-map)
    #_(compile-web-resources assets-out)))

;; (log/trace "loaded miraj/compiler.clj")
