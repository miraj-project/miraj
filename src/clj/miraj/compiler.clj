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
                      '[miraj.co-dom                         :as x]
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

(log/trace "loading miraj/compiler.clj")

(stencil.loader/set-cache (clojure.core.cache/ttl-cache-factory {} :ttl 0))

(def ^:dynamic *debug* false)
(def ^:dynamic *keep* false)
(def ^:dynamic *pprint* false)
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

;; one html loader file per pagespace. all pages defpaged in one
;; pagespace will use the same loader.
(defn link-pages
  "Link pages - generate html loader and *.cljs.edn files."
  ;; go thru pages, pulling the required component libs, then generate files
  [nss-syms verbose]
  (if *verbose* (log/info "link-pages: " nss-syms))
  (doseq [pagespace-sym nss-syms]
    (clojure.core/require pagespace-sym)
    (let [pagespace-ns (find-ns pagespace-sym)]
      ;; (log/debug "DEFPAGE FOUND?" (-> pagespace-ns meta :miraj/miraj :miraj/defpage))
      (if (-> pagespace-ns meta :miraj/miraj :miraj/defpage)
        (do
          (log/info (format "NS %s is a defpage space" pagespace-ns))
          (let [ns-map-nss (map #(-> % second meta :ns)
                                (filter #(-> % second meta :miraj/miraj) (ns-map pagespace-ns)))
                ;; _ (log/debug "NS-MAP: " ns-map-nss)
                ns-refers-nss (map #(-> % second meta :ns)
                                   (filter #(-> % second meta :miraj/miraj) (ns-refers pagespace-ns)))
                ;; _ (log/debug "NS-REFERS: " ns-refers-nss)
                ns-aliases-nss (map #(-> % second)
                                    (filter #(-> % second meta :miraj/miraj) (ns-aliases pagespace-ns)))
                ;; _ (log/debug "NS-ALIASES: " ns-aliases-nss)

                component-nss (set (concat ns-map-nss ns-refers-nss ns-aliases-nss))
                _ (log/debug "COMPONENT NSS: " component-nss)

              ;; 1. iterate over component-nss, pulling out the :miraj/nss and converting to hrefs
                impl-nss (flatten (remove nil? (for [component-ns component-nss]
                                                 (do #_(log/debug (format "NS %s" component-ns))
                                                     #_(log/debug (format "META: %s"
                                                                        (-> component-ns meta :miraj/miraj :miraj/nss)))
                                                     (let [nss (-> component-ns meta
                                                                   :miraj/miraj :miraj/nss)]
                                                       (if (empty? nss) nil nss))))))
                _ (log/debug (format "IMPL_NSS %s" (seq impl-nss)))

                pagelib-path (utils/sym->path pagespace-sym)
                      ;; _ (log/debug "PAGELIB-PATH: " pagelib-path)

                ;; FIXME: take :base-path into account
                js-path (str pagelib-path "/js")
                _ (log/debug (format "JS-PATH %s" js-path))

                ;; FIXME: one module per page, not per component in page!
                ;; :entries #{} must contain all the component nss for the page module
                ;; modules (into {} (merge-with concat (for [impl-ns impl-nss]
                ;;           (let [[pfx nm] (modularize-ns impl-ns)
                ;;                 module {(keyword nm) {:output-to (str js-path "/" nm ".js")
                ;;                         ;(str (sym->path pfx) "/js")
                ;;                                       :entries
                ;;                                       #{impl-ns}}}] ;;impl-ns
                ;;             (log/debug (format "MODULE %s" module))
                ;;             module))))
                modules {(keyword pagespace-sym) {:output-to js-path
                                                  :entries (into #{} impl-nss)}}
                _ (log/debug (format "MODULES %s" modules))

                codom-maps (vec (map (fn [ns]
                                       {:loader (str "/" (utils/sym->path ns) ".html")})
                                     impl-nss))
                _ (log/debug "CODOM-MAPS: " codom-maps)

                interns (ns-interns pagespace-ns)
                ;; _ (log/debug "INTERNS: " interns)
                ;; _ (doseq [i (seq interns)]
                ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                defpage-vars (map second
                                 (filter #(-> % second meta :miraj/miraj :miraj/defpage)
                                         (seq interns)))
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
                (let [;;js-path (str pagelib-path "/js")
                      edn-content {:require (apply vector impl-nss)
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
            ))))))

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
                                       #(-> % find-ns meta :miraj/miraj :miraj/defpage)
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
                                       :demonstrates (-> p meta :miraj/demonstrates)
                                       :title (-> p meta :miraj/miraj :html/title)
                                       :desc (-> p meta :miraj/miraj :html/description)}) pages)))))))
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
                              impl-nss
                              (vec
                               (map (fn [v]
                                      (-> v meta :miraj/miraj :miraj/assets :miraj/impl-nss))
                                    ;;path->ns-sym))
                                    component-vars))
                              _ (log/debug "IMPL-NSS: " impl-nss)


                              js-path (str base-path "/js")
                              edn-content {:require (apply vector impl-nss)
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
        href (utils/sym->path (-> component-var meta :miraj/miraj :miraj/assets :miraj/impl-nss))
        out-file (str/join "/" [*compile-path* (str href ".html")])]
        ;; out-file (str/join "/" [*compile-path* path (str (-> component-var meta :name) ".html")])]
    (io/make-parents out-file)
    (if *verbose* (log/info (format "Emitting %s" out-file)))
    (spit out-file (with-out-str (if *pprint* (x/pprint codom) (x/serialize codom))))))
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
        (compile-webcomponent-var-html component-var *pprint* *verbose*)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;        PAGE COMPILE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn compile-page-nss
  "Process all page vars in a namespace."
  ([page-nss-syms]
   (compile-page-nss page-nss-syms false false))
  ([page-nss-syms pprint]
   (compile-page-nss page-nss-syms pprint false))
  ([page-nss-syms pprint verbose]
   (if *verbose* (log/info (format "Running fn compile-page-nss for %s" page-nss-syms)))
   (doseq [page-ns-sym (seq page-nss-syms)]
     (log/debug "Processing pagespace:" page-ns-sym)
     ;; we need :reload to get interactive dev
     ;; FIXME: make this dependent on :debug flag
     (if *debug*
       (clojure.core/require page-ns-sym :reload)
       (clojure.core/require page-ns-sym))
     (let [page-ns (find-ns page-ns-sym)
           ;; _ (log/debug "Page NS: " page-ns)
           page-vars (vals (into {}
                                 (let [interns-map (ns-interns page-ns)]
                                   (filter (fn [entry]
                                             ;;(log/debug "xxxx: " (-> entry second meta :miraj/miraj))
                                             (-> entry second meta :miraj/miraj :miraj/defpage))
                                           ;;(get (meta (last entry)) :miraj/defpage))
                                           interns-map))))]
       ;; (log/debug "page vars: " page-vars)
       (doseq [page-var page-vars]
         ;; (log/debug (format "PAGE VAR %s" page-var))
         ;; (log/debug (format "PAGE VAR META %s" (-> page-var meta)))
         (let [page-name (-> page-var meta :name)
               page-ns (-> page-var meta :ns)
               base-path (-> page-var meta :miraj/miraj :miraj/base-path)
               _ (log/debug (format "BASE path %s" base-path))
               path (if base-path base-path (utils/ns->path (-> page-var meta :ns)))
               hfile (str/join "/" [*compile-path* path (str page-name ".html")])
               ;; cfile (str/join "/" [cljs-out path (str page-name ".cljs")])
               ]
           (io/make-parents hfile)
           (binding [*ns* page-ns]
             (if *verbose* (log/info (format "Compiling page %s" page-var)))
             (let [out-str (if *pprint*
                             (with-out-str (-> page-var normalize optimize x/pprint))
                             (with-out-str (-> page-var
                                               normalize optimize x/serialize print)))]
               (if *verbose* (log/debug (format "Emitting %s" hfile)))
               (spit hfile out-str)))))))))

(defn compile-page-ref
  "Compile defpage var to html+js and write to *compile-path*."
  [page-ref]
  ;; ([page-ref]
  ;;  (compile-page-ref page-ref false))
  ;; ([page-ref pprint verbose]
  (log/debug (format "compile-page-ref *verbose* %s" *verbose*))
  (if *verbose* (log/debug "COMPILE-PAGE-REF: " page-ref (var? page-ref)))
  (if *verbose* (log/debug "COMPILE-PAGE-REF meta: " (-> page-ref meta)))

  ;; inject polyfill, so imports will work
  ;; inject link import page/imports.html, unless compile-only is specified

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
       (if *verbose* (log/info (format "Compiling %s to %s" page-ref hfile)))
       (let [out-str (if *pprint*
                       (with-out-str (-> page-ref normalize optimize x/pprint))
                       (with-out-str (-> page-ref
                                         normalize ;; pass a switch to inject import link?
                                         optimize
                                         x/serialize
                                         print)))]
         (if *verbose* (log/info (format "Emitting %s" hfile)))
         (spit hfile out-str)))))

(defn add-polyfill
  [page-ref polyfill]
  (log/debug (format "add-polyfill %s %s" page-ref polyfill))
  (log/debug (format "page meta %s" (-> page-ref meta)))
  (if (not (contains? #{:lite :heavy} polyfill))
    (throw (Exception.
            (format "unrecognized :polyfill param: %s; use :lite or :heavy" polyfill))))
  (alter-meta! page-ref (fn [old new]
                          (log/debug (format "OLD %s" old))
                          (assoc-in old [:miraj/miraj :miraj/polyfill] new))
               polyfill)
  (log/debug (format "page meta 2 %s" (-> page-ref meta))))

;; analogous to gcc - call it mcc, miraj compiler collection?
(defn mcc   ;;  compile
  [& {:keys [page pages namespaces
             optimizations compile-only
             imports polyfill
             debug]
      :as opts}]
  (log/info (format "mcc %s" opts))
  (let [namespaces (or namespaces (if (and page (nil? pages)) nil :all))
        pages (or pages (if (and page (nil? namespaces)) nil :all))]
    ;; (log/debug (format "Compile (fixed): page %s, nss %s, pages %s" page namespaces pages))

    ;; if page var and both nss and pages are :all, do just page
    (if page
      (let [[page-ref page-ns page-var page-sym]
            (if (var? page)
              [page (-> page meta :ns) page nil]
              (if (symbol? page)
                (if (namespace page)
                  ;; a var sym
                  (let [pvar (clojure.core/resolve page)
                        pns (-> pvar meta :ns)]
                    [pvar pns pvar page])
                  ;; must be an ns sym
                  (do
                    ;; for interactive dev we need to reload the ns
                    ;; should this be the responsibility of the dev?
                    ;; no, the boot task should do this
                    ;;(clojure.core/require page :reload)
                    (let [pns (find-ns page)]
                      [pns pns nil page])))
                ;; must be an ns
                (do [page page nil (page ns-name)])))]
        (log/info "Page ref: " page-ref)
        (log/info "Page ns: " page-ns)
        (log/info "Page var: " page-var)
        (log/info "Page sym: " page-sym)
        ;; reloading page is client responsibility - boot task or repl
        ;; (clojure.core/require (-> page-ns ns-name) :reload)
        (if polyfill (add-polyfill page-ref polyfill))
        (if imports (alter-meta! page-ref (fn [old new]
                                            (assoc-in old [:miraj/miraj :miraj/imports] new))
                                 imports))
        (log/info (format "Compiling page %s" page-ref))
        (compile-page-ref page-ref)))
    (if namespaces
      (do (log/debug (format "Compiling nss %s" namespaces))
          (if pages (log/debug (format "Compiling page names %s" pages)))))))

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

(log/trace "loaded miraj/compiler.clj")
