(ns ^{:doc "Miraj web compile functions"
      :author "Gregg Reynolds"}
  miraj.compiler
  (:refer-clojure :exclude [compile import refer require])
  (:require [clojure.string                       :as str]
            [clojure.pprint                       :as pp]
            [clojure.data.json                    :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.tools.namespace.repl         :as ctnr :refer [refresh set-refresh-dirs]]
            [clojure.pprint                       :as pp]
            [clojure.java.io                      :as io]
            [clj-time.core                        :as t]
            [stencil.core                         :as stencil]
            [boot.core                            :as boot]
            ;; [boot.pod                          :as pod]
            ;; [boot.util                         :as util]
            ;; [mobileink.boot-bowdlerize         :as bow]
            [miraj.core                           :as miraj]
            [miraj.co-dom                         :as x]
            ;; [clojure.tools.reader              :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer                     :as ana]
            ;; [cljs.compiler                     :as c]
            ;; [cljs.closure                      :as cc]
            ;; [cljs.env                          :as env])
            [clojure.tools.logging                :as log :only [trace debug error info]])
  (:import [java.io FileNotFoundException StringWriter]))

           ;;  ByteArrayInputStream StringReader
           ;; [javax.xml.stream XMLInputFactory
           ;;                   XMLStreamReader
           ;;                   XMLStreamConstants]
           ;; [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           ;; [javax.xml.transform.dom DOMSource]
           ;; [javax.xml.transform OutputKeys TransformerFactory]
           ;; [javax.xml.transform.stream StreamSource StreamResult]
           ;; [java.nio.charset Charset]
           ;; [java.io Reader]
           ;; [miraj NSException]))
;;           [java.util Date]))

;;(println "loading miraj/compiler.clj")

(defn ns->path [namesp]
  ;; (println "ns->path: " namesp)
  (let [ns-name (ns-name namesp)
        ;; _ (println "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (println "path: " path)
        ;; fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (println "fn: " fn)
        ]
    ;; (str path "/" fn)))
    path))

(defn sym->path [sym]
  ;; (println "ns->path: " namesp)
  (let [path (str/replace sym #"-|\." {"-" "_" "." "/"})
        ;; _ (println "path: " path)
        ;; fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (println "fn: " fn)
        ]
    ;; (str path "/" fn)))
    path))

(defn var->path [v]
  (let [ns-str (str (-> v meta :ns ns-name))
        ns-path (str/replace ns-str #"-|\." {"-" "_" "." "/"})
        name (-> v meta :name)
        ]
    (str ns-path "/" name)))

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

(defn- bowerize
  [bower-pkg uri cache & verbose]
  (println "    BOWERIZE " bower-pkg cache)
  ;; (let [resource (io/file (.getPath cache) uri)]
  (let [resource (io/file cache uri)]
    (if (.exists resource)
      (if verbose (println "bowerize: found cached " uri))
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
          (if verbose (println (format "bowerize: installing bower pkg:   %s\n" bower-pkg)))
          (apply sh c))))))


;; finds and templates defcomponent vars
;; for deflibrary vars use link-component-libs
;; for defcomponents we use a deflibrary listing the nss to search for defcomponents
(defn assemble-component-lib-for-ns
  "Assemble a library of webcomponents."
  [component-ns-sym pprint verbose]
  (if verbose (log/debug (format "Assembling lib for webcomponents in ns: %s" component-ns-sym)))
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

(defn get-component-maps-for-ns
  "get config maps for components in ns"
  [component-ns-sym pprint verbose]
  ;; (if verbose (log/debug (format "get-component-maps-for-ns: %s" component-ns-sym)))
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
                     ;;(into {}
                     (let [m (-> component-var meta)
                           ;; _ (log/debug "COMPVAR META: " m)
                           href (str (ns->path (:ns m)) "/" (sym->path (:name m)) ".html")
                           elt-fn-meta (update-in m [:miraj/miraj]
                                                  (fn [old]
                                                    (let [;oldmiraj (:miraj/miraj old)
                                                          miraj (assoc
                                                                 {:miraj/defn (symbol (:name m))
                                                                  :miraj/co-fn true
                                                                  :miraj/element true
                                                                  :miraj/lib :miraj/demo
                                                                  :miraj/assets {:miraj/href
                                                                                 (str
                                                                                  (-> old
                                                                                      :miraj/assets
                                                                                      :miraj/base)
                                                                                  ".html")
                                                                                 :miraj/scripts
                                                                                 (str
                                                                                  (-> old
                                                                                      :miraj/assets
                                                                                      :miraj/base)
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

(defn link-component-cljs
  [deflib-var verbose]
  ;; (log/debug "link-component-cljs deflib var: " deflib-var)
  ;; (log/debug "link-component-cljs deflib requires: " (-> deflib-var deref))
  ;; (if verbose (log/info "Generating component cljs: " ))
  (let [component-nss (map first (-> deflib-var deref :miraj/require))
        ;; _ (log/debug "Component nss: " component-nss)
        _ (doseq [component-ns component-nss] (clojure.core/require component-ns :reload))
        component-vars (flatten (miraj/get-component-vars component-nss))
        ;; _ (log/debug "Component vars: " component-vars)
        edn-requires (map (fn [component-var]
                            [component-var (-> component-var meta :miraj/miraj :miraj/html-tag)])
                          component-vars)
        edn-require-nss (for [edn-require edn-requires]
                          (let [ns (-> edn-require first meta :ns ns-name)
                                html-tag (-> edn-require second)]
                            (symbol (str ns "." html-tag))))
        ;; _ (log/debug "edn-require-nss: " edn-require-nss)
        ;; path (ns->path (-> deflib-var meta :ns ns-name))
        path (var->path deflib-var)
        ;;FIXME: support :optimizations, etc.
        edn-content {:require (apply vector edn-require-nss)
                               #_[(symbol (str/join "." [(-> component-var meta :ns)
                                                      (-> component-var meta
                                                          :miraj/miraj :miraj/html-tag)
                                                      ;; (-> component-var meta :name)
                                                      "core"]))]
                     :init-fns []
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
      (if verbose (log/info (format "Emitting %s" edn-file)))
      (io/make-parents edn-file)
      (spit edn-file edn-content))))

(defn get-component-nss
  [deflib-var & verbose]
  ;; (log/debug "get-component-nss for deflib var: " deflib-var)
  ;; (log/debug "link-component-cljs deflib requires: " (-> deflib-var deref))
  ;; (if verbose (log/info "Generating component cljs: " ))
  (let [component-nss (map first (-> deflib-var deref :miraj/require))
        ;; _ (log/debug "Component nss: " component-nss)
        _ (doseq [component-ns component-nss] (clojure.core/require component-ns :reload))
        component-vars (flatten (miraj/get-component-vars component-nss))
        ;; _ (log/debug "Component vars: " component-vars)
        edn-requires (map (fn [component-var]
                            [component-var (-> component-var meta :miraj/miraj :miraj/html-tag)])
                          component-vars)
        edn-require-nss (for [edn-require edn-requires]
                          (let [ns (-> edn-require first meta :ns ns-name)
                                html-tag (-> edn-require second)]
                            (symbol (str ns "." html-tag))))]
    (log/debug "edn-require-nss: " edn-require-nss)
    edn-require-nss))

(defn link-component-libs
  "Link webcomponent libraries. This generates the .clj file
  containing element fns for components, plus an html loader file."
  [nss-syms verbose]
  (if verbose (log/info "link-component-libs: " nss-syms))
  (doseq [deflibspace-sym nss-syms]
    (clojure.core/require deflibspace-sym)
    (let [deflibspace-ns (find-ns deflibspace-sym)
          component-lib-name (-> deflibspace-ns meta :name)]
      (if (-> deflibspace-ns meta :miraj/miraj :miraj/deflibrary)
        (do
          ;; (log/info (format "NS %s is a deflibrary space" deflibspace-ns))
          (let [interns (ns-interns deflibspace-ns)
                ;; _ (log/debug "INTERNS: " interns)
                ;; _ (doseq [i (seq interns)]
                ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                deflib-vars (map second
                                 (filter #(-> % second meta :miraj/miraj :miraj/deflibrary)
                                         (seq interns)))]
            (doseq [deflib-var deflib-vars]
              #_(log/debug "Processing deflibrary: " deflib-var (-> deflib-var meta :miraj/miraj))
              (let [component-lib-ns-sym (str deflibspace-ns
                                              "." (-> deflib-var meta :name))]
                (if (= :miraj/elements (-> deflib-var meta :miraj/miraj :miraj/deflibrary))
                  (do
                    ;; (link-component-cljs deflib-var verbose)
                    (if verbose (log/info "Generating component library: " component-lib-ns-sym))
                    (if-let [ns-vectors (-> deflib-var deref :miraj/require)]
                      (let [component-maps (flatten (for [ns-vector ns-vectors]
                                                      (do
                                                        ;;(log/debug "Processing :miraj/require: " ns-vector)
                                                        (clojure.core/require (first ns-vector))
                                                        (get-component-maps-for-ns (first ns-vector) true true))))]
                        (log/debug "COMPONENT-MAPS: " component-maps)
                        ;;(doseq [component-map component-maps]
                        (let [lib-clj-path (sym->path component-lib-ns-sym)
                              ;; _ (log/debug "LIB-CLJ-PATH: " lib-clj-path)
                              lib-clj-file (str lib-clj-path ".clj")
                              lib-js-file (str lib-clj-path ".js")
                              lib-nss (vec (get-component-nss deflib-var))
                              html-loader-file (str "/" lib-clj-path "-import.html")

                              component-defns (stencil/render-file
                                               "miraj/templates/webcomponents.mustache"
                                       {:miraj/ns component-lib-ns-sym
                                        :miraj/codom html-loader-file
                                        :miraj/nss lib-nss
                                        :miraj/base {:miraj/assets {:miraj/href "/"}}
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
                          (log/debug (format "writing component lib to %s" component-out-path))
                          (spit component-out-file component-defns)
                          ;; (log/debug (format "writing component loader to %s" html-loader-path))
                          ;; (spit html-loader-out-file html-loader)
                          ))
                      ;; else
                      (assoc (deref deflib-var) :miraj/ns component-lib-ns-sym)))
                    (comment "process :miraj/styles here"))
                  ))))))))

;; one html loader file per pagespace. all pages defpaged in one
;; pagespace will use the same loader.
(defn link-pages
  "Link pages"
  [nss-syms verbose]
  (if verbose (log/info "link-pages: " nss-syms))
  (doseq [pagespace-sym nss-syms]
    (clojure.core/require pagespace-sym)
    (let [pagespace-ns (find-ns pagespace-sym)
          component-lib-name (-> pagespace-ns meta :name)]
      ;; (log/debug "DEFPAGE FOUND?" (-> pagespace-ns meta :miraj/miraj :miraj/defpage))
      (if (-> pagespace-ns meta :miraj/miraj :miraj/defpage)
        (do
          ;; (log/info (format "NS %s is a defpage space" pagespace-ns))
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
                ;; _ (log/debug "COMPONENT NSS: " component-nss)

              ;; 1. iterate over component-nss, pulling out the :miraj/nss and converting to hrefs
                cljs-nss (flatten (remove nil? (for [component-ns component-nss]
                                         (-> component-ns meta :miraj/miraj :miraj/nss))))
                codom-maps (vec (map (fn [ns] {:loader (str "/" (sym->path ns) ".html")}) cljs-nss))
                _ (log/debug "CODOM-MAPS: " codom-maps)

                interns (ns-interns pagespace-ns)
                ;; _ (log/debug "INTERNS: " interns)
                ;; _ (doseq [i (seq interns)]
                ;;     (log/debug "meta: " (-> i second meta :miraj/miraj :miraj/deflibrary)))
                defpage-vars (map second
                                 (filter #(-> % second meta :miraj/miraj :miraj/defpage)
                                         (seq interns)))
                pagelib-path (sym->path pagespace-sym)
                      ;; _ (log/debug "PAGELIB-PATH: " pagelib-path)
                pagelib-file (str pagelib-path ".clj")
                pagelib-js-file (str pagelib-path ".js")]
            (if (not (empty? codom-maps))
              (do
                ;; 1. write the loader file
                (let [;; must match import link href in core/normalize
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
                  (log/debug (format "writing component loader to %s" html-loader-path))
                  (spit html-loader-out-file html-loader))
                ;; 2. write cljs.edn file
                (let [js-path (str pagelib-path "/js")
                      edn-content {:require (apply vector cljs-nss)
                                   :compiler-options {:optimizations :none
                                                      :asset-path (str "/" js-path)
                                                      :output-dir js-path
                                                      ;; must match core/normalize
                                                      :output-to  (str js-path "/components.js")
                                                      }}
                      ;; (println "edncontent: " edncontent)
                      edn-file (str/join "/" [*compile-path* (str pagelib-path ".cljs.edn")])
                        ;; edn-file (str/join "/" [*compile-path* path (str name ".cljs.edn")])
                        ]
                    (if verbose (log/info (format "Emitting %s" edn-file)))
                    (io/make-parents edn-file)
                    (spit edn-file edn-content))))
            ))))))

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
  (let [refsets (miraj/get-miraj-vars-all-nss)
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
                  true))))) ;; verbose

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
            (bowerize pkg-name uri cache verbose))))))

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
        imports-config-map (miraj/get-imports-config-map)]
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
              (compile-polymer-component component imports-config-map assets-out verbose)
              (compile-required-component component imports-config-map assets-out verbose))))))))

(defn compile-webcomponent-var-html
  "Compile defcomponent var to html and write to files."
  [component-var pprint verbose]
  (log/debug "compile-webcomponent-var-html: " component-var)
  (let [path (ns->path (-> component-var meta :ns))
        ;; _ (println "PATH: " path)
        name (str/replace (-> component-var meta :name) #"-" "_")
        codom (-> component-var meta :miraj/miraj :miraj/codom)
        href (sym->path (-> component-var meta :miraj/miraj :miraj/assets :miraj/base))
        out-file (str/join "/" [*compile-path* (str href ".html")])]
        ;; out-file (str/join "/" [*compile-path* path (str (-> component-var meta :name) ".html")])]
    (io/make-parents out-file)
    (if verbose (log/info (format "Emitting %s" out-file)))
    (spit out-file (with-out-str (if pprint (x/pprint codom) (x/serialize codom))))))

#_(defn compile-webcomponent-var-cljs
  "Compile defcomponent var to cljs and write to files."
  [component-var pprint verbose]
  (println "compile-webcomponent-var-cljs: " component-var)
  (let [path (ns->path (-> component-var meta :ns))
        ;; _ (println "PATH: " path)
        name (str/replace (-> component-var meta :name) #"-" "_")
        codom (-> component-var meta :miraj/miraj :miraj/codom)
        href (-> component-var meta :miraj/miraj :miraj/assets :miraj/href)
        cljs-file (str/join "/" [*compile-path* (sym->path href) ".cljs"])
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
                                       :output-to  (str (sym->path href) ".js")
                                       ;; :output-to  (str path "/" name ".js")
                                       }}] ;;FIXME: support :optimizations, etc.
    ;; (println "edncontent: " edncontent)
    (if verbose (log/info (format "Emitting %s" cljs-file)))
    (io/make-parents cljs-file)
    (io/make-parents edn-file)
    (spit edn-file edn-content)
    (spit cljs-file (-> component-var meta :miraj/miraj :miraj/prototype))))

(defn compile-webcomponents-cljs
  "Compile defcomponent vars to cljs and write to files.
  This does *not* create the .edn.cljs that boot-cljs needs. That is the job of the link fn."
  [nss pprint verbose]
  (log/debug "compile-webcomponents-cljs: " nss)
  ;;(ctnr/refresh)
  (let [component-vars (flatten (miraj/get-component-vars nss))
        ;; _ (log/debug "Component vars: " component-vars)
        component-nss (set (map (fn [v] (-> v meta :ns)) component-vars))
        ;; _ (log/debug "Component nss: " component-nss)
        ]
    (doseq [component-var component-vars]
      (let [path (ns->path (-> component-var meta :ns))
            ;; _ (println "PATH: " path)
            ;; codom (-> component-var meta :miraj/miraj :miraj/codom)
            href (-> component-var meta :miraj/miraj :miraj/assets :miraj/base)
            cljs-file (str *compile-path* "/" (sym->path href) ".cljs")] ;; was: /core.cljs
        (if verbose (log/info (format "Emitting %s" cljs-file)))
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

(defn compile-webcomponent-nss
  "Compile all webcomponents in namespaces."
  [html-or-cljs component-nss pprint verbose]
  (if verbose (log/debug (format "Compiling %s for webcomponents in %s" html-or-cljs component-nss)))
  (if (not (or (= :html html-or-cljs) (= :cljs html-or-cljs)))
    (throw (Exception. "First arg must be one of :html or :cljs")))
  ;; first do html
  (if (= :html html-or-cljs)
    (doseq [component-ns component-nss]
      (clojure.core/require component-ns)
      (let [component-ns (find-ns component-ns)
            component-vars (miraj/get-component-vars-for-ns component-ns)]
        (doseq [component-var component-vars]
          (compile-webcomponent-var-html component-var pprint verbose)))))
  (if (= :cljs html-or-cljs)
    (compile-webcomponents-cljs component-nss pprint verbose)))

(defn compile-page-nss
  "Process all page vars in a namespace."
  ([page-nss-syms]
   (compile-page-nss page-nss-syms false false))
  ([page-nss-syms pprint]
   (compile-page-nss page-nss-syms pprint false))
  ([page-nss-syms pprint verbose]
   (if verbose (log/info (format "compile-page-nss for %s" page-nss-syms)))
  (doseq [page-ns-sym (seq page-nss-syms)]
    (log/debug "Processing page ns sym:" page-ns-sym)
    (clojure.core/require page-ns-sym) ;; :reload)
    (let [page-ns (find-ns page-ns-sym)
          _ (log/debug "Page NS: " page-ns)
          page-vars (vals (into {}
                            (let [interns-map (ns-interns page-ns)]
                              (filter (fn [entry]
                                        ;;(log/debug "xxxx: " (-> entry second meta :miraj/miraj))
                                        (-> entry second meta :miraj/miraj :miraj/defpage))
                                        ;;(get (meta (last entry)) :miraj/defpage))
                                      interns-map))))]
      (log/debug "page vars: " page-vars)
      (doseq [page-var page-vars]
        (let [page-name (-> page-var meta :name)
              page-ns (-> page-var meta :ns)
              path (ns->path (-> page-var meta :ns))
              hfile (str/join "/" [*compile-path* path (str page-name ".html")])
              ;; cfile (str/join "/" [cljs-out path (str page-name ".cljs")])
              ]
          (log/debug "html loader: " hfile)
          (io/make-parents hfile)
          (binding [*ns* page-ns]
            (if verbose (log/info (format "Compiling page %s" page-var)))
            (let [out-str (if pprint
                            (with-out-str (-> page-var miraj/normalize miraj/optimize x/pprint))
                            (with-out-str (-> page-var
                                              miraj/normalize miraj/optimize x/serialize print)))]
              (spit hfile out-str)))
          #_(spit cfile (-> page-var meta :miraj :prototype))))))))

(defn compile-page-var
  "Compile defpage var to html+js and write to *compile-path*."
  ([page-var]
   (compile-page-var page-var false))
  ([page-var pprint verbose]
   ;; (if verbose (log/debug "COMPILE-PAGE-VAR: " page-var))
   (let [page-var-name (str (:name (meta page-var)))
         page-var-ns (-> page-var meta :ns)
         html-path (ns->path page-var-ns)
         page-html-name (str html-path "/" page-var-name ".html")
         hfile (str/join "/" [*compile-path* page-html-name])]
     (io/make-parents hfile)
     (binding [*ns* page-var-ns]
       (if verbose (log/info (format "Compiling %s to %s" page-var hfile)))
       (clojure.core/require (ns-name page-var-ns)) ; :reload)
       (let [out-str (if pprint (with-out-str (-> page-var miraj/normalize miraj/optimize x/pprint))
                         (with-out-str (-> page-var
                                           miraj/normalize miraj/optimize x/serialize print)))]
         (spit hfile out-str))))))

  (defn webdeps
  "Download and cache web resource dependencies."
  [cache & verbose]
  (println "webdeps ")
  (binding [*ns* *ns*]
    (println "REFRESHING")
    (let [e (ctnr/refresh)]
      (if (= e :ok)
        (println "refresh result: " e "\n")
        (do ;; (clojure.repl/pst)
          (println "Exception " (.getMessage e))
            (throw e)))) ;;(Exception. "foo")))))
    (compile-web-resources cache verbose)))

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
    #_(miraj/get-imports-config-map)
    #_(compile-web-resources assets-out)))

;;(println "loaded miraj/compiler.clj")
