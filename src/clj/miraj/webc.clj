(ns ^{:doc "Miraj web compile functions"
      :author "Gregg Reynolds"}
  miraj.webc
  (:refer-clojure :exclude [import refer require])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.tools.namespace.repl :as repl :refer [refresh set-refresh-dirs]]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clj-time.core :as t]
            [boot.core :as boot]
            [boot.pod :as pod]
            [boot.util :as util]
            ;; [mobileink.boot-bowdlerize :as bow]
            [miraj.core :as mrj]
            [miraj.markup :as x])
            ;; [clojure.tools.reader :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer :as ana]
            ;; [cljs.compiler :as c]
            ;; [cljs.closure :as cc]
            ;; [cljs.env :as env])
            ;; [clojure.tools.logging :as log :only [trace debug error info]])
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

(println "loading miraj/webc.clj")

(defn- get-webvars
  "Search namespaces for vars"
  [web-var & nss]
  (println "get-webvars: " web-var nss)
  (let [nss (or nss (all-ns))]
    (let [webvars (for [the-ns nss]
                    (do
                      (let [interns-map (ns-interns the-ns)]
                        (filter (fn [entry] (get (meta (last entry)) web-var)) interns-map))))]
      webvars)))

(defn- bowerize
  [bower-pkg uri cache & verbose]
  ;; (println "bowerize " bower-pkg cache)
  (let [resource (io/file (.getPath cache) uri)]
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
          ;; (println "bower cmd: " c)
          (if verbose (println (format "bowerize: installing bower pkg:   %s\n" bower-pkg)))
          (apply sh c))))))

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

(defn- compile-polymer-component
  "Compile required web component forms."
   [import imports-config-map cache & verbose]
  (println "compile-polymer-component: " import)
  ;; (println "imports-config-map: " imports-config-map)
  (let [pns (first import)
        items (apply hash-map (next import))
        segs (str/split (str pns) #"\.")
        seg1 (first segs)
        seg2 (fnext segs)]
    ;; (println "segs: " segs)
    ;; (println "seg1: " seg1)
    ;; (println "seg2: " seg2)
    (if (not= seg1 "polymer") (throw (Exception. "trying to polymer-compile non-polymer component " import)))
    (doseq [pvar (:refer items)]
      ;; (println "PVAR: " pvar)
      (let [v (find-var (symbol (str pns) (str pvar)))
            uri (:uri (meta v))]
        ;; (println "VAR: " v)
        ;; (println "URI: " uri)
          (let [pkg-name (str "PolymerElements/" seg2 "-" pvar)]
            ;; (println "IMPORT POLYMER: " pkg-name)
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
  (let [pages (apply hash-map (flatten (remove empty? (apply get-webvars :_webpage nil))))
        imports-config-map (mrj/get-imports-config-map)]
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
            (if (= seg1 "polymer")
              (compile-polymer-component component imports-config-map assets-out verbose)
              (compile-required-component component imports-config-map assets-out verbose))))))))

(defn- compile-webcomponent
  "Compile defweb-component forms to html+js and write to files."
   [csym cvar html-out cljs-out]
  ;; [^clojure.lang.Symbol component & mode]
  ;; [file doc & mode]
  (println "compile-webcomponent: " csym cvar)
  (println "Output dirs: " html-out cljs-out)
  (let [path (str/replace (str (:ns (meta cvar))) #"\." "/")
        cfsym (str/replace csym #"-" "_")
        hfile (str/join "/" [html-out path (str cfsym ".html")])
        cfile (str/join "/" [cljs-out path cfsym "core.cljs"])
        ednfile (str/join "/" [cljs-out path (str cfsym ".cljs.edn")])
        edncontent {:require [(symbol (str/join "." [(:ns (meta cvar)) csym "core"]))]
                    :init-fns []
                    :compiler-options {:optimizations :advanced}}] ;;FIXME: support :optimizations, etc.
    (println "hfile: " hfile)
    (println "cfile: " cfile)
    (println "edncontent: " edncontent)
    (io/make-parents hfile)
    (io/make-parents cfile)
    (io/make-parents ednfile)
    (spit hfile (with-out-str (x/pprint (-> cvar meta :miraj :codom))))
    (spit ednfile edncontent)
    (spit cfile (-> cvar meta :miraj :prototype))))

;;FIXME: only compile webcomponents actually used in a webpage
(defn- compile-webcomponents
  "Process all webcomponents in the project."
  [html-out cljs-out & nss]
  (println "compile-webcomponenents, html-out: " html-out "; cljs-out: " cljs-out ", nss: " nss)
  (let [components (apply hash-map (flatten (remove empty?
                                                    (apply get-webvars :_webcomponent nss))))]
    ;;(println "webcomponents: " components)
    (doseq [c components]
      (compile-webcomponent (first c) (last c) html-out cljs-out))))

(defn- compile-webpage
  "Compile defweb-page forms to html+js and write to files."
   [csym cvar html-out cljs-out]
  ;; [^clojure.lang.Symbol component & mode]
  ;; [file doc & mode]
  (println "compile-webpage: " csym cvar)
  (println "Output dirs: " html-out cljs-out)
  (let [path (str/replace (str (:ns (meta cvar))) #"\." "/")
        csym (str/replace csym #"-" "_")
        hfile (str/join "/" [html-out path (str csym ".html")])
        cfile (str/join "/" [cljs-out path (str csym ".cljs")])
        web-ns (.. cvar -ns -name)]
    (println "hfile: " hfile)
    ;; (println "web-ns: " web-ns)
    ;; (println "webpage: " (do (in-ns web-ns) (x/pprint (var-get cvar))))
    (io/make-parents hfile)
    ;; (io/make-parents cfile)
    (in-ns web-ns)
    (spit hfile (with-out-str (-> cvar x/normalize x/pprint)))
    #_(spit cfile (-> cvar meta :miraj :prototype))))

(defn- compile-webpages
  "Process all webpages in the project."
  [html-out cljs-out & nss]
  (println "compile-webpages, html-out: " html-out "; cljs-out: " cljs-out ", nss: " nss)
  (let [pages (apply hash-map (flatten (remove empty? (apply get-webvars :_webpage nss))))]
    (println "webpages: " pages)
    (doseq [page pages]
      (compile-webpage (first page) (last page) html-out cljs-out))))

(defn webdeps
  "Download and cache web resource dependencies."
  [cache & verbose]
  (println "webdeps ")
  (binding [*ns* *ns*]
    (println "REFRESHING")
    (let [e (repl/refresh)]
      (if (= e :ok)
        (println "refresh result: " e "\n")
        (do ;; (clojure.repl/pst)
          (println "Exception " (.getMessage e))
            (throw e)))) ;;(Exception. "foo")))))
    (compile-web-resources cache verbose)))

(defn webc
  "Compile clj+cljs to html+js. Optional args: html-out (string),
  cljs-out (string) nss (vector of ns symbols). With no args, process
  all namespaces and write output to './'.  Each namespace will be
  searched for defcomponent and defpage forms, which will be processed
  to generate HTML and Javascript files.

  Also write bowdlerize.edn containing bower configs."
  [& {:keys [html-out cljs-out assets-out nss]
      :or   {html-out "./"
             cljs-out   "./"
             assets-out "./"
             nss nil}}]
  (println "webc " *ns*)
  (binding [*ns* *ns*]
    (println "REFRESHING")
    (let [e (repl/refresh)]
      (if (= e :ok)
        (println "refresh result: " e "\n")
        (do ;; (clojure.repl/pst)
          (println "Exception " (.getMessage e))
            (throw e)))) ;;(Exception. "foo")))))
    (compile-webcomponents html-out cljs-out)
    (compile-webpages html-out cljs-out)
    (mrj/get-imports-config-map)
    #_(compile-web-resources assets-out)))

(println "loaded miraj/webc.clj")
