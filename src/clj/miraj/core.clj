;   (Portions) Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Includes chunks of code borrowed from clojure.core, hence portions
; copyright Rich Hickey.

(ns ^{:doc "Miraj core functions"
      :author "Gregg Reynolds"}
  miraj.core
  (:refer-clojure :exclude [import refer require])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clj-time.core :as t]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [boot.core :as boot]
            [boot.pod :as pod]
            [boot.util :as util]
            ;; [mobileink.boot-bowdlerize :as bow]
            ;; [polymer :refer :all]
            [miraj.co-dom :as x]
            ;; [clojure.tools.reader :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer :as ana]
            ;; [cljs.compiler :as c]
            ;; [cljs.closure :as cc]
            ;; [cljs.env :as env])
            [clojure.tools.logging :as log :only [trace debug error info]])
  (:import [java.io FileNotFoundException StringWriter]))

(def ^:dynamic *miraj-sym* (gensym "miraj"))

;; (in-ns 'clojure.core)
;; (defmacro miraj-ns
;;   [nm & opts]
;;   (log/debug "expanding co-ns"))

(in-ns 'miraj.core)

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

;; (log/debug "loading miraj/core.clj")

(def bower-repo "bower_components")

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

(def cljkey->jskey
  {:notify :notify
   :reflect :reflectToAttribute
   :read-only :readOnly})

(def cljtype->jstype
  {'Boolean 'js/Boolean
   'Date 'js/Date
   'Number 'js/Number
   'String 'js/String
   'Vector 'js/Array
   'Map 'js/Object})

(defn ns->uri [n]
  ;; (log/debug "NS->URI")
  (str/replace n #"\." "/"))

(defn ns->path [namesp]
  ;; (log/debug "ns->path: " namesp)
  (let [ns-name (ns-name namesp)
        ;; _ (log/debug "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "path: " path)
        ;; fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "fn: " fn)
        ]
    ;; (str path "/" fn)))
    path))

(defn ns-sym->path [ns-sym]
  ;; (log/debug "ns-sym->path: " ns-sym)
  (let [ns-name (str ns-sym)
        ;; _ (log/debug "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "path: " path)
        ;; fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "fn: " fn)
        ]
    ;; (str path "/" fn)))
    path))

(defn var->path [v]
  ;; (log/debug "var->path: " v)
  (let [nm (:name (meta v))
        namesp (:ns (meta v))
        ;; _ (log/debug "namesp: " namesp)
        ns-name (ns-name namesp)
        ;; _ (log/debug "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "path: " path)
        fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (log/debug "fn: " fn)
        ]
    (str path "/" fn)))

(defn sym->cljs-ns
  [sym]
  ;; (log/debug "sym->cljs-ns: " sym)
  (let [v (resolve sym)
        nm (:name (meta v))
        ;; _ (log/debug "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (log/debug "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defn var->cljs-ns
  [v]
  ;; (log/debug "var->cljs-ns: " v (-> v meta))
  (let [nm (:name (meta v))
        ;; _ (log/debug "nm: " nm)
        ;; namesp (:ns (meta v))
        ;; _ (log/debug "namesp: " namesp)
        ns-name (-> v meta :ns ns-name)
        ]
    (symbol (str ns-name "." nm))))

(defn var->sym
  [v]
  ;; (log/debug "var->sym: " v)
  (let [nm (:name (meta v))
        ;; _ (log/debug "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (log/debug "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defmacro interface-sym->protocol-sym
  "Protocol names cannot contain '.'"
  [sym]
  ;; (log/debug "interface-sym->protocol-sym: " sym)
  `(if (var? (resolve ~sym))
     (symbol (str (->> (resolve ~sym) meta :ns))
            (str (->> (resolve ~sym) meta :name)))
     (let [nodes# (str/split (str ~sym) #"\.")
           ns# (str/join "." (drop-last nodes#))
           ;; _# (log/debug "ns: " ns#)
           nm# (last nodes#)
           newsym# (symbol (str ns#) (str nm#))]
       ;; (log/debug "interface-sym->protocol-sym: " ~sym " => " newsym#)
       newsym#)))

(def polymer-prop-descriptors
  {:type "type", :value "value", :notify "notify", :read-only "readOnly",
   :reflect-to-attribute "reflectToAttribute", :observer "observer", :computed "computed"})

(defn construct-properties-js
  "Translate props to Polymer property expressions"
  [props]
  (str "properties: {\n\t    "
       (str/join ",\n\t    "
                 (for [prop props]
                   (str prop ": {"
                        (str/join ", " (for [[k v] (meta prop)]
                                         (let [nm (get polymer-prop-descriptors k)]
                                           (if nm
                                             (str nm ": " (pr-str v))
                                             (throw (Exception. (str "Invalid property descriptor: " k)))))))
                        "}")))
       "\n\t  }"))

(defn optimize-js
  [doc]
  ;; (log/debug "JS optimizer: " doc)
  (with-meta
    (x/xsl-xform x/xsl-optimize-js doc)
    (meta doc)))

  ;; (let [doc-zip (zip/xml-zip doc)]
  ;;   (seq (-> doc-zip zip/down))

  ;;   ))

(defn optimize-css
  [doc]
   (log/debug "CSS optimizer"))

(defn optimize
  ;;FIXME handle null strategy correctly
  [strategy & doc]
  ;; (log/debug "    OPTIMIZE: " strategy);  " :: " doc)
  (reset! x/mode :html)

  (case strategy
    :js (apply optimize-js doc)
    :css (apply optimize-css doc)
    (if (keyword? strategy)
      (throw (Exception. (str "Unrecognize optimization strategy: " strategy)))
      (if (nil? doc)
        (optimize-js strategy)
        (optimize-js doc)))))
;;    (log/debug "Unrecognized optimizer: " strategy)))

(defn get-component-vars
  "Search namespaces for webcomponents"
  [nss]
  ;; (log/debug "get-componenent-vars for nss: " nss)
  ;;(let [nss (or nss (all-ns))]
  (for [the-ns (seq nss)]
    (do
      (clojure.core/require the-ns)
      (let [interns-map (ns-interns the-ns)
            component-vars (filter (fn [entry]
                                    (-> entry last meta :miraj/miraj :miraj/component))
                                  interns-map)]
        (map second component-vars)))))

(defn get-component-vars-for-ns
  "Find all component vars in namespace."
  [component-ns]
  ;; (log/debug "get-component-vars-for-ns: " component-ns)
  #_(binding [*ns* component-ns]
    (doseq [n (ns-interns component-ns)]
      (log/debug "INTERN: " n)
      (log/debug (-> n second meta :miraj/miraj :miraj/component))))
  (binding [*ns* component-ns]
    (let [component-vars (vals (into {}  (filter (fn [r] (-> r second meta :miraj/miraj :miraj/component))
                                                 (ns-interns component-ns))))
          ]
      component-vars)))

(defn get-component-vars-all-nss
  "Find all miraj vars with :miraj/assets metadata, so we can construct link
  elts."
  [page-ns]
  (binding [*ns* page-ns]
    (let [all-nss (all-ns)
          ;; _ (doseq [n all-nss] (log/debug "XXXX NSS: " n))
          miraj-nss (filter (fn [x] (-> x meta :miraj/miraj)) (all-ns))
          refs (ns-refers page-ns)] ;;*ns*)]
      #_(doseq [miraj-ns miraj-nss]
          (if (empty? (filter #(= miraj-ns (:ns (meta (last %)))) refs))
            ;; the :require clause did NOT use :refers
            (doseq [[k v] (filter #(not (empty? (-> (meta (last %)) :miraj/miraj :miraj/assets)))
                                  (ns-publics miraj-ns))]
              (log/debug "key: " k ", v: " v))
            ;; the :require clause DID use :refers
            (doseq [[k v]
                    (filter #(= miraj-ns (:ns (meta (last %)))) refs)]
              (log/debug "Key: " k ", v: " v))))
      (for [miraj-ns miraj-nss]
        (do #_(log/debug "  MIRAJ NS: " miraj-ns)
            (let [miraj-refs (filter #(= miraj-ns (:ns (meta (last %)))) refs)]
              (if (empty? miraj-refs)
                (let [miraj-all-refs
                      (filter #(not (empty? (-> (meta (last %)) :miraj/miraj :miraj/assets)))
                              (ns-publics miraj-ns))]
                  ;;(filter #(= miraj-ns (:ns (meta (last %)))) (ns-publics miraj-ns))]
                  #_(log/debug "    MIRAJ-REFS for " miraj-ns " count: " (count miraj-all-refs))
                  miraj-all-refs)
                (do #_(log/debug "    MIRAJ-REFS for " miraj-ns ": " miraj-refs)
                    miraj-refs))))))))

(defn get-miraj-vars-all-nss
  "Find all miraj vars with :miraj/assets metadata, so we can construct link
  elts."
  [page-ns]
  (binding [*ns* page-ns]
    (let [all-nss (all-ns)
          ;; _ (doseq [n all-nss] (log/debug "XXXX NSS: " n))
          miraj-nss (filter (fn [x] (-> x meta :miraj/miraj)) (all-ns))
          refs (ns-refers page-ns)] ;;*ns*)]
      #_(doseq [miraj-ns miraj-nss]
          (if (empty? (filter #(= miraj-ns (:ns (meta (last %)))) refs))
            ;; the :require clause did NOT use :refers
            (doseq [[k v] (filter #(not (empty? (-> (meta (last %)) :miraj/miraj :miraj/assets)))
                                  (ns-publics miraj-ns))]
              (log/debug "key: " k ", v: " v))
            ;; the :require clause DID use :refers
            (doseq [[k v]
                    (filter #(= miraj-ns (:ns (meta (last %)))) refs)]
              (log/debug "Key: " k ", v: " v))))
      (for [miraj-ns miraj-nss]
        (do #_(log/debug "  MIRAJ NS: " miraj-ns)
            (let [miraj-refs (filter #(= miraj-ns (:ns (meta (last %)))) refs)]
              (if (empty? miraj-refs)
                (let [miraj-all-refs
                      (filter #(not (empty? (-> (meta (last %)) :miraj/miraj :miraj/assets)))
                              (ns-publics miraj-ns))]
                  ;;(filter #(= miraj-ns (:ns (meta (last %)))) (ns-publics miraj-ns))]
                  #_(log/debug "    MIRAJ-REFS for " miraj-ns " count: " (count miraj-all-refs))
                  miraj-all-refs)
                (do #_(log/debug "    MIRAJ-REFS for " miraj-ns ": " miraj-refs)
                    miraj-refs))))))))

(defn get-miraj-vars-for-pagespace
  "Find all referred miraj vars with :assets metadata, so we can construct link
  elts."
  [page-ns]
  ;; (log/debug "get-miraj-vars-for-pagespace: " page-ns)
  (binding [*ns* page-ns]
    (let [refs (vals (into {}  (filter (fn [r] (-> r second meta :miraj/miraj)) (ns-refers page-ns))))
          ;; _ (log/debug "REF VARS: " refs)

          ref-nss (set (map #(-> % meta :ns) refs))
          ;; _ (log/debug "ref-nss: " ref-nss)
          ;; _ (doseq [n ref-nss] (log/debug "REF NS: " n))

          ;; miraj-nss (set (map second (filter #(-> % second meta :miraj/miraj) (ns-aliases page-ns))))

          ;; _ (doseq [x (ns-map page-ns)] (log/debug (format "%s : %s (%s)"
          ;;                                                  (-> x second meta :ns) (second x)
          ;;                                                  (type (second x)))))

          aliases  (filter (fn [[k v]] (-> v meta :miraj/miraj)) (ns-aliases page-ns))
          ;; _ (log/debug "aliases: " aliases)

          ;; _ (doseq [alias aliases]
          ;;     (log/debug "Checking alias " (second alias))
          ;;     (let [hit (filter #(do (log/debug %)
          ;;                            (= (second alias) %)) ref-nss)]
          ;;       (log/debug hit)
          ;;       (if (empty? hit) alias hit)))

          noref-aliases (remove empty? (for [alias aliases]
                                          (let [hit (filter #(= (second alias) %) ref-nss)]
                                            (if (empty? hit) alias nil))))
          ;; _ (log/debug "noref-aliases: " noref-aliases)

          noref-alias-vars (vals (first (for [noref-alias noref-aliases]
                                          (ns-publics (second noref-alias)))))
          noref-alias-vars (filter #(-> % meta :miraj/miraj :miraj/co-fn) noref-alias-vars)
          ;; _ (log/debug "noref-alias-vars: " noref-alias-vars)

          all-map ;;(map #(-> % second meta :ns)
          (filter #(and (var? (second %))
                        (not= 'clojure.core
                              (-> % second meta :ns ns-name)))
                  (ns-map page-ns)) ;;)
          ;; _ (log/debug "all-map: " all-map)

          miraj-nss (set (filter #(-> % meta :miraj/miraj) all-map))
          ;; _ (doseq [n miraj-nss] (log/debug "MIRAJ NS: " n))

          noref-nss (set/difference miraj-nss ref-nss)
          ;; _ (doseq [n noref-nss] (log/debug "NOREF NS: " n))

          noref-nss (if (empty? noref-nss) noref-nss
                     (filter #(-> % meta :miraj/miraj :miraj/co-fn)
                             (vals (into {} (map ns-publics noref-nss)))))
          ;; _ (log/debug "noref-miraj-nss: " noref-nss)

          allrefs (concat refs noref-nss noref-alias-vars)
          ]
      ;; (doseq [refvar allrefs] (log/debug " REFVAR: " refvar))
      allrefs)))

(defn normalize
  "inspect webpage var, if necessary create <head> etc."
  [page-var]
  (log/debug "Normalizing HTML page-var: " page-var)
  ;; (log/debug "Normalize HTML meta: " (meta page-var))
  (reset! x/mode :html)
  (let [page-ns (-> page-var meta :ns)
        page-ns-sym (ns-name page-ns)
        page-name (-> page-var meta :name)
        refs (get-miraj-vars-for-pagespace page-ns)

        ;; _ (log/debug "REFS: " refs)
        ;; _ (doseq [refvar refs]
        ;;       (log/debug (-> refvar meta :miraj/miraj :miraj/assets :miraj/href)))
        miraj-libs (set (filter (fn [e] (and
                                        (-> e meta :miraj/miraj :miraj/elements)
                                        ;; FIXME: how to distinguish between 3rd party and miraj?
                                        (-> e meta :miraj/miraj :miraj/assets :miraj/base)))
                               (set (map (fn [r] (-> r meta :ns)) refs))))
        ;; _ (log/debug "LIBS: " miraj-libs)

        refs (filter (fn [r] (let [rns (-> r meta :ns)] (not (contains? miraj-libs rns))))
                     refs)

        links (for [refvar refs]
                (x/element :link {:rel "import"
                                  :href (str (-> refvar meta :miraj/miraj :miraj/assets :miraj/href))}));)

        ;; href must match html-loader path in compiler/link-pages
        miraj-links (for [lib miraj-libs]
                      (x/element :link {:rel "import"
                                        :href (str "/" (ns->path page-ns) "/miraj-imports.html")}))
                                        ;; :href (str "miraj/" *miraj-sym* "-import.html")}))
                                  ;; :href (str (-> lib meta :miraj/miraj :miraj/assets :miraj/href))}))

        script-links (if (not (empty? miraj-links)) ;; (for [lib miraj-libs]
                       (x/element :script {:src (str "/" (ns->path page-ns) "/js/components.js")}))
                                           ;; #_(str (-> lib meta
                                           ;;            :miraj/miraj
                                           ;;            :miraj/assets
                                           ;;            :miraj/script))}))

        ;; _ (log/debug "PAGE META: " (meta page-var))
        ;; html-metas (dissoc (meta page-var) :doc :name :ns :miraj/miraj)
        html-metas (-> page-var meta :miraj/miraj)
        ;; _ (log/debug "HTML METAS: " html-metas)
        meta-elts (x/get-metas html-metas)
        ;; _ (log/debug "    META-ELTS: " meta-elts)

        page-content (deref page-var)
        ;; _ (log/debug "PAGE CONTENT: " page-content)

        header (first (->> page-content :content (filter #(= (:tag %) :head))))
        ;; _ (log/debug "    HEADER CONTENT: " header)

        body (binding [*ns* (-> page-var meta :ns)]
               (first (->> page-content :content (filter #(= (:tag %) :body)))))
        ;; _ (log/debug "    BODY CONTENT: " body)

        newheader (apply x/element :head {} (vec (flatten
                                                  (list meta-elts
                                                        (x/element :script {:type "text/javascript"
                                                                            :src "/bower_components/webcomponentsjs/webcomponents-lite.min.js"})
                                                        links
                                                        miraj-links
                                                        script-links
                                                        (:content header)))))
        ;; _ (log/debug "NEW HEADER: " newheader)
        normed (x/element :html newheader body)
        ;;normh (binding [*ns* page-ns] (x/xsl-xform x/xsl-normalize page-content))
        ;; _ (log/debug (format "NORMALIZED: %s" normed))
        ]
    normed))

(defn normalize-methods
  [methods]
  ;; (log/debug "NORMALIZE-METHODS")
  (into {} (for [method methods]
             {(if (= 'with-element (first method))
                (keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                (keyword (first method)))
              (if (= 'with-element (first method))
                [(keyword (gensym (str "_" (name (-> method next first)) "." (-> method nnext first first))))
                 (apply list 'fn (-> method nnext first next))]
                [(keyword (gensym (str "_" (first method))))
                 (apply list 'fn (rest method))])})))

(defn normalize-rawmethods
  [methods]
  ;; (log/debug "NORMALIZE-RAWMETHODS")
  (into {} (for [method methods]
             {(if (= 'with-element (first method))
                (keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                (keyword (first method)))
              (if (= 'with-element (first method))
                [(keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                 (apply list 'fn (-> method nnext first next))]
                [(keyword (str (first method)))
                 (apply list 'fn (rest method))])})))

(defn protos->rawbehaviors
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-behaviors
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (log/debug "FILTERED BEHAVIORS: " ls)
    (if (seq ls)
      {:behaviors
       (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
         (if (nil? proto)
           result
           (let [proto (interface-sym->protocol-sym proto)
                 ;; _ (log/debug "BEH META: " (meta (resolve proto)))
                 resource-type (:resource-type (meta (resolve proto)))
                 resource-name (:resource-name (meta (resolve proto)))
                 methods (take-while seq? tail)
                 next-proto (drop-while seq? tail)]
             (recur (first next-proto)
                    (next next-proto)
                    (if (= :polymer-behaviors resource-type)
                      (merge result {resource-name (let [ms (normalize-methods methods)]
                                                     ;; (log/debug "NORMED BEHS: " ms)
                                                     ms)})
                      result)))))})))

(defn protos->rawlisteners
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (log/debug "FILTERED LISTENERS: " ls)
    (if (seq ls)
      {:listeners
       (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
         (if (nil? proto)
           result
           (let [proto (interface-sym->protocol-sym proto)
                 resource-type (:resource-type (meta (resolve proto)))
                 methods (take-while seq? tail)
                 next-proto (drop-while seq? tail)]
             (recur (first next-proto)
                    (next next-proto)
                    (if (= :polymer-events resource-type)
                      (merge result {proto (let [ms (normalize-methods methods)]
                                             ;; (log/debug "NORMED LISTENERS: " ms)
                                             ms)})
                      result)))))})))

(defn protos->rawmethods
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (:co-protocol?
                                 (meta (resolve (interface-sym->protocol-sym p))))))
                   protos)]
    ;; (log/debug "FILTERED METHODS: " ls)
    (if (seq ls)
      (let [ms {:methods
                (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
                  (if (nil? proto)
                    result
                    (let [proto (interface-sym->protocol-sym proto)
                          resource-type (:resource-type (meta (resolve proto)))
                          methods (take-while seq? tail)
                          next-proto (drop-while seq? tail)]
                      (recur (first next-proto)
                             (next next-proto)
                             (if (:co-protocol? (meta (resolve proto)))
                               (merge result {proto (let [ms (normalize-rawmethods methods)]
                                                      ;; (log/debug "NORMED METHS: " ms)
                                                      ms)})
                               result)))))}]
        ;; (log/debug "METHODS X: " ms)
        ms))))

(defn protos->behaviorvec
  [protos]
  ;; (log/debug "PROTOS->BEHAVIORVEC: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-behaviors
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (log/debug "FILTERED BEHAVIORS: " ls)
    (if (seq ls)
      {:listeners
       (into []
                     ;;(doall
                     (loop [proto (first protos)
                            tail (rest protos)
                            result ""]
                       (if (nil? proto)
                         result
                         (let [proto (interface-sym->protocol-sym proto)
                               resource-type (:resource-type (meta (resolve proto)))]
                           ;; (log/debug "LISTENER PROTO: " proto)
                           ;; (log/debug "LISTENER TYPE: " resource-type)
                           ;; (log/debug "LISTENER TAIL: " tail)
                           ;; (log/debug "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (log/debug "LISTENER METHODS: " methods (type methods))
                             (log/debug "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (log/debug "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (log/debug "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (log/debug "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (log/debug "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-behaviors resource-type)
                                        (concat result meths)
                                        result))))))))
           })))

(defn construct-behaviors-js
  [protocols]
  ;; (log/debug "CONSTRUCT-BEHAVIORS-JS: " protocols)
  (let [protos (filter (fn [p] (if (not (seq? p))
                                 (and
                                  (not (seq? p))
                                  (= :polymer-behaviors
                                     (:resource-type
                                      (meta (resolve (interface-sym->protocol-sym p))))))))
                       protocols)]
    (if (seq protos)
      (str "behaviors: [\n\t    "
           (str/join ",\n\t    "
                     (for [proto protos #_(filter (fn [p]
                                           (if (not (seq? p))
                                             (and
                                              (not (seq? p))
                                              (= :polymer-behaviors
                                                 (:resource-type
                                                  (meta (resolve (interface-sym->protocol-sym p))))))))
                                         protos)]
                       (do
                         (log/debug "P META: " proto
                                  (meta (resolve (interface-sym->protocol-sym proto))))

                         (str (:resource-name (meta (resolve
                                                     (interface-sym->protocol-sym proto))))))))
           "\n\t  ]"))))

(defn construct-listeners-js
  [protos]
  (log/debug "CONSTRUCT-LISTENERS-JS: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    (log/debug "FILTERED LISTENERS: " ls)
    (if (seq ls)
      (str "listeners: {\n\t    "
           (str/join ",\n\t    "
                     ;;(doall
                     (loop [proto (first protos)
                            tail (rest protos)
                            result ""]
                       (if (nil? proto)
                         result
                         (let [proto (interface-sym->protocol-sym proto)
                               resource-type (:resource-type (meta (resolve proto)))]
                           (log/debug "LISTENER PROTO: " proto)
                           (log/debug "LISTENER TYPE: " resource-type)
                           (log/debug "LISTENER TAIL: " tail)
                           (log/debug "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (log/debug "LISTENER METHODS: " methods (type methods))
                             (log/debug "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (log/debug "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (log/debug "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (log/debug "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (log/debug "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-events resource-type)
                                        (concat result meths)
                                        result))))))))
           "\n\t  }"))))

(defn- props->propmap
  [args]
  ;; (log/debug "PROPS->PROPMAP: " args)
  (let [props (filter (fn [x] (and (symbol? x)
                                       (:properties (meta (resolve x))))) args)]
    ;; (log/debug "PROPS: " props)
    (if props
      (let [properties (into {} (for [prop props]
                                  (do ;; (log/debug "PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (log/debug "PVAR: " pvar)
                                            ps (:props (deref pvar))
                                            ;; _ (log/debug "PS: " ps)
                                            ]
                                        ps))))
            html-attrs (into {} (for [prop props]
                                  (do ;; (log/debug "HTMLATTRS PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (log/debug "PVAR: " pvar)
                                            html-attrs (:html-attrs (deref pvar))
                                            ;; _ (log/debug "HTMLATTRS: " html-attrs)
                                            ]
                                        html-attrs))))]
        {:properties properties :html-attrs html-attrs}))))

(declare behaviors->elements)

(defn get-proto-codefs
  "proto names are actually js prototype names; polymer behaviors
  lookup table is predefined in polymer.behaviors, user-defined behaviors must be configured in a namespace matching the proto name, e.g. MyBehaviors.HighlightBehavior is configured in MyBehaviors.clj"
  ;; NB: a co-def is a ref to def code, e.g. clojure :require clauses are co-defs
  ;; here, <link> refs to impl code are polymer behavior (protocol) co-defs
  ;; 1. verify protocols exist
  ;; 2. validate method impl sigs
  ;; 3. generate <link> markups
  [opts+specs]
  (log/debug "GET-PROTO-CODEFS: " opts+specs)
  (let [links (behaviors->elements opts+specs)]
    ;;[interfaces methods opts] (behaviors->elements opts+specs)]
    ;; (log/debug "INTERFACES: " interfaces)
    ;; (log/debug "METHODS: " methods)
    ;; (log/debug "OPTS: " opts)
    links))

(defn- parse-cotype-args
  [args]
  ;; (log/debug "PARSE COTYPE ARGS: " (pr-str args))
  (let [[docstr args] (if (string? (first args))
                        [(first args) (rest args)]
                        ["" args])
        codom (let [cd (filter (fn [x] (if (symbol? x)
                                         (:miraj/codom (meta (resolve x))))) args)]
                (if cd
                  cd ;; else get the (codom ...) form
                  ))
        _ (if (> (count codom) 1) (throw (IllegalArgumentException. (str "Only one codom arg allowed"))))
        props (let [props (filter (fn [x] (if (symbol? x)
                                            (:properties (meta (resolve x))))) args)]
                (if props
                  props
                  ))
        ;; _ (log/debug "PROPERTIES: " props)
        ;; exclude docstring and codom
        protos (filter (fn [x] (if (or (list? x)
                                       (and (symbol? x)
                                            (not (:miraj/codom (meta (resolve x))))))
                                 x)) args)
        ;; behaviors
        ]
    [docstr args codom protos]))

;; from clojure/core_deftype.clj
(defn- parse-opts [s]
  ;; (log/debug "parse-opts: " s)
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  ;; (log/debug "PARSE-IMPLS: " specs)
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn coprotocol?
  [maybe-p]
  (:co-protocol? (meta (resolve maybe-p))))

(defn protocol?
  [maybe-p]
  (boolean (:on-interface maybe-p)))

(defn- ->protomap [opts+specs]
  "emit elements for protos"
  (log/debug "->protomap: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        _ (log/debug "OPTS: " opts)
        _ (log/debug "SPECS: " specs)
        impls (parse-impls (first specs))
        _ (log/debug "IMPLS: " impls (count impls))
        sigs (into {} (map (fn [arg]
                             (let [_ (log/debug "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        _ (log/debug "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               (log/debug "PROTO: " psym)
                               (log/debug "PROTO var: " (resolve psym))
                               (log/debug "meta PROTO var: " (meta (resolve psym)))
                               (log/debug "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        _ (log/debug "URIs: " uris)
        interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        _ (log/debug "INTERFACES: " interfaces)
        _ (doseq [intf interfaces] (log/debug "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (log/debug "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    (doseq [[k v] impls]
      (let [proto (interface-sym->protocol-sym k)
            sig (get sigs proto)]
        (log/debug "PARSING PROTOCOL: " sig)
        (doseq [method-impl v]
          (log/debug "interface sym: " k)
          (log/debug "method-impl: " method-impl)
          (let [method-kw (if (= 'with-element (first method-impl))
                            (keyword (first (first (nnext method-impl))))
                            (keyword (first method-impl)))
                _ (log/debug "method-kw: " method-kw)
                method-sig (get sig method-kw)
                method-impl (if (= 'with-element (first method-impl))
                              (next (first (nnext method-impl)))
                              method-impl)]
            (log/debug "method-impl: " method-impl)
            (log/debug "method-sig: " method-sig)
            (if (nil? method-sig)
              (if (not= 'This k)
                (throw (Exception. (str "Method '" (first method-impl) "' "
                                        "not declared in protocol '" proto "'"))))
              ;; if arity not correct throw bad arity exception
              ;; if fnext is fn, then fnext of next should be arg vector
              ;;FIXME impl-arity
              (let [impl-arity 1
                    proto-arities (set (->> (:arglists method-sig)
                                           (map count)))]
                (log/debug "PROT-ARITIES: " proto-arities)
                #_(if (not-any? proto-arities [impl-arity])
                    (throw (Exception. (str "Bad arity: " method-impl " v. " method-sig))))))))))
    (for [[proto uri] uris]
      (if uri
        (x/element :link {:rel "import" :href uri})
        (x/element :link {:rel "import" :href (str proto)})))))

(defn- behaviors->elements [opts+specs]
  "emit elements for protos"
  ;; (log/debug "behaviors->elements: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        ;; _ (log/debug "OPTS: " opts)
        ;; _ (log/debug "SPECS: " specs)
        impls (parse-impls (first specs))
        ;; _ (log/debug "IMPLS: " impls (count impls))
        ;; obtain sig defns by resolving the protocol symbol
        sigs (into {} (map (fn [arg]
                             (let [;;_ (log/debug "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        ;; _ (log/debug "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               ;; (log/debug "PROTO: " psym)
                               ;; (log/debug "PROTO var: " (resolve psym))
                               ;; (log/debug "meta PROTO var: " (meta (resolve psym)))
                               ;; (log/debug "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        ;; _ (log/debug "URIs: " uris)
        ;; interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
        ;;                set
        ;;                (disj 'Object 'java.lang.Object)
        ;;                vec)
        ;; _ (log/debug "INTERFACES: " interfaces)
        ;; _ (doseq [intf interfaces] (log/debug "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (log/debug "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    ;; (doseq [[k v] impls]
    ;;   (let [proto (interface-sym->protocol-sym k)
    ;;         sig (get sigs proto)]
    ;;     (log/debug "PARSING PROTOCOL: " sig)
    ;;     (doseq [method-impl v]
    ;;       (log/debug "interface sym: " k)
    ;;       (log/debug "method-impl: " method-impl)
    ;;       (let [method-kw (if (= 'with-element (first method-impl))
    ;;                         (keyword (first (first (nnext method-impl))))
    ;;                         (keyword (first method-impl)))
    ;;             _ (log/debug "method-kw: " method-kw)
    ;;             method-sig (get sig method-kw)
    ;;             method-impl (if (= 'with-element (first method-impl))
    ;;                           (next (first (nnext method-impl)))
    ;;                           method-impl)]
    ;;         (log/debug "method-impl: " method-impl)
    ;;         (log/debug "method-sig: " method-sig)
    ;;         (if (nil? method-sig)
    ;;           (if (not= 'This k)
    ;;             (throw (Exception. (str "Method '" (first method-impl) "' "
    ;;                                     "not declared in protocol '" proto "'"))))
    ;;           ;; if arity not correct throw bad arity exception
    ;;           ;; if fnext is fn, then fnext of next should be arg vector
    ;;           ;;FIXME impl-arity
    ;;           (let [impl-arity 1
    ;;                 proto-arities (set (->> (:arglists method-sig)
    ;;                                        (map count)))]
    ;;             (log/debug "PROT-ARITIES: " proto-arities)
    ;;             #_(if (not-any? proto-arities [impl-arity])
    ;;                 (throw (Exception. (str "Bad arity: " method-impl " v. " method-sig))))))))))
    (for [[proto uri] uris]
      (if uri
        (x/element :link {:rel "import" :href uri})
        (x/element :link {:rel "import" :href (str proto)})))))

(def property-types
  {'Vector ^{:doc " (i.e. satisfy vector?)"} (fn [x] (vector? x))
   'Map ^{:doc " (i.e. satisfy map?)"} (fn [x] (map? x))
   'Boolean ^{:doc "  Allowed values are 'true' and 'false'."} (fn [x] (or (= 'true x) (= 'false x)))
   'Date ^{:doc "[year month? day? hour? minute? second? millisecond?] (see clj-time) "} (fn [x]
           (log/debug "DATE ARG: " x (type x))
           (and (vector? x)
                (<= (count x) 7)
                (not (empty? x))
                (every? number? x)))
   'Number number?
   'String string?})

;; from clojure/core_deftype.clj
;; reduce is defined again later after InternalReduce loads
(defn ^:private ^:static
  reduce1
       ([f coll]
             (let [s (seq coll)]
               (if s
         (reduce1 f (first s) (next s))
                 (f))))
       ([f val coll]
          (let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
         val))))

(defn- assert-same-protocol [protocol-var method-syms]
  (doseq [m method-syms]
    (let [v (resolve m)
          p (:protocol (meta v))]
      (when (and v (bound? v) (not= protocol-var p))
        (binding [*out* *err*]
          (log/debug "Warning: protocol" protocol-var "is overwriting"
                   (if p
                     (str "method " (.sym v) " of protocol " (.sym p))
                     (str "function " (.sym v)))))))))

(defn- emit-properties [name opts+sigs]
  (let [iname (symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname)} sigs opts+sigs]
          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        ;; _ (log/debug "PRESIGS: " sigs)
        html-attrs (first (filter map? sigs))
        ;; _ (log/debug "HTMLATTRS: " html-attrs)
        sigs (filter list? sigs)
        ;; _ (log/debug "LIST PRESIGS: " sigs)
        all-props (->> (map first (filter #(:tag (meta (first %))) sigs)))
        ;; _ (log/debug "all-props: " all-props)
        sigs (when sigs
               (reduce1 (fn [m s]
                          ;; (log/debug "REDUCE1 M: " m)
                          ;; (log/debug "REDUCE1 S: " s)
                          (let [name-meta (meta (first s))
                                ;; _ (log/debug "name-meta: " name-meta)
                                name-meta (dissoc (assoc name-meta :type (:tag name-meta))
                                                              :tag)
                                ;; _ (log/debug "name-meta: " name-meta)
                                doc (:doc name-meta)
                                prop-type (if (some (set [(:type name-meta)])
                                                    (set (keys property-types)))
                                              (:type name-meta)
                                              'Observer)
                                ;; _ (log/debug "prop-type: " prop-type)

                                name-meta (if (= prop-type 'Observer)
                                            (assoc name-meta :type 'Observer)
                                            name-meta)
                                ;; _ (log/debug "name-meta: " name-meta)
                                mname (with-meta (first s) nil)
                                ;; _ (log/debug "mname: " mname)

                                _ (let [strs (filter string? s)]
                                    ;; (log/debug "STRINGS: " strs)
                                    (if (= prop-type 'String)
                                      (if (> (count strs) 1)
                                              (throw (IllegalArgumentException.
                                                      (str "Too many String args for property: " mname))))
                                      (if (not= prop-type 'Observer)
                                        (if (> (count strs) 0)
                                          (throw (IllegalArgumentException.
                                                  (str "Illegal String arg for property: " mname)))))))

                                default-val (let [v (if (= :computed (first (rest s)))
                                                      (if (fn? (eval (first (nnext s))))
                                                        (do ;;(log/debug "COMPUTED " (first (nnext s)))
                                                            (first (nnext s)))
                                                        (throw (IllegalArgumentException.
                                                                (str "Computed value for "
                                                                     mname
                                                                     " must be a fn."))))
                                                      (if (= prop-type 'Observer)
                                                        nil
                                                        (first (rest s))))]
                                              #_(log/debug "V: " v (fn? (eval v)))
                                              (if (list? v) ;; a fn?
                                                (if (= (first v) 'fn)
                                                  (first (nnext v))
                                                  v)
                                                #_(throw (Exception. (str "don't understand " v))))
                                              (if (= prop-type 'Date)
                                                v
                                                #_(.toString (apply t/date-time v))
                                                v))
                                ;; _ (log/debug "default-val: " default-val (type default-val))
                                _ (if (not= prop-type 'Observer)
                                    (if (not (nil? default-val))
                                      (let [pred (get property-types prop-type)]
                                        ;; (log/debug "pred: " pred)
                                        (if (fn? (eval default-val))
                                          (if (not (= :computed (first (rest s))))
                                            (throw (IllegalArgumentException. (str "Default value "
                                                                                   (pr-str default-val)
                                                                                   " for "
                                                                                   mname
                                                                                   " must match type "
                                                                                   prop-type
                                                                                   ". "
                                                                                   (:doc (meta pred))))))
                                          (if (not (apply pred [default-val]))
                                            (throw (IllegalArgumentException. (str "Default value "
                                                                                   (pr-str default-val)
                                                                                   " for "
                                                                                   mname
                                                                                   " must match type "
                                                                                   prop-type
                                                                                   ". "
                                                                                   (:doc (meta pred))))))))))
                                flags (filter keyword? s)
                                ;; _ (log/debug "flags: " flags)
                                _  (if (some #{:computed} flags)
                                     (if (not (= :computed (nth s 1)))
                                       (throw (Exception. (str "Flag :computed must be first arg: "
                                                               mname)))))
                                observer (let [obs (if (some #{:computed} flags)
                                                     (do ;;(log/debug "COMPUTED HIT")
                                                         (filter list? (next (nnext s))))
                                                     (if (= prop-type 'Observer)
                                                       (list (conj (rest s) 'fn))
                                                       (filter list? s)))]
                                           obs)
                                ;; doc (let [ss (filter string? s)]
                                ;;       (if (= 2 (count ss))
                                ;;         (last ss)
                                ;;         (if (= (first (next s)) (first ss)) nil (first ss))))
                                ;; _ (log/debug "observer: " observer) ; (-> (first observer) next first))
                                ;; _ (log/debug "doc: " doc)
                                ]
                            (when (> (count observer) 1)
                              (throw (IllegalArgumentException. (str "Only one observer allowed for property " mname))))
                            (if (not (empty? observer))
                              (if (not= prop-type 'Observer)
                                (let [argcount (count (-> (first observer) next first))]
                                  (when (not= 2 argcount)
                                    (throw (IllegalArgumentException.
                                            (str "Definition of observer function for property "
                                                 mname
                                                 " must take exactly two args, for new and old vals, in that order.")))))))

                            (cond
                              (= prop-type 'Observer)
                              (if (not (empty? flags))
                                (throw (IllegalArgumentException. (str "Flags "
                                                                       (pr-str flags)
                                                                       " not allowed on multi-prop observer: " mname)))
                                (let [args (fnext (first observer))]
                                  ;; (log/debug "OBSERVER ARGS: " args)
                                  (if (empty? args)
                                    (throw (IllegalArgumentException. (str "Argument vector for multi-prop observer " mname " must not be empty."))))
                                  (if (not (every? (set all-props) args))
                                    (throw (IllegalArgumentException. (str "Argument vector for multi-prop observer " mname " must contain property names:" args)))))))

                            (when (m (keyword mname))
                              (throw (IllegalArgumentException. (str "Function " mname " in protocol " name " was redefined. Specify all arities in single definition."))))


                            (assoc m (keyword mname)
                                   (merge name-meta
                                          {:value default-val}
                                           {:flags flags}
                                           ;; :raw s
                                          {:name (vary-meta mname assoc :doc doc :observer observer)}
                                           (if (not (empty? observer))
                                             {:observer [(keyword (gensym (str "_" mname)))
                                                              (first observer)]})
                                           (if (not (nil? doc))
                                           {:doc doc})))))
                        {} sigs)) ;; end reduce1

        ;; _ (log/debug "SIGS: " sigs)
        ;; meths (mapcat (fn [sig]
        ;;                 (let [m (munge (:name sig))]
        ;;                   (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object)
        ;;                        (:observer sig))))
        ;;               (vals sigs))
        ;; _ (log/debug "METHS: " meths)
        ]
    ;; (log/debug "DEFPROPS A")
  `(do
     (defonce ~name {})
     ;; (gen-interface :name ~iname :methods ~meths)
     (alter-meta! (var ~name) assoc :doc ~(:doc opts) :properties true)
     ~(when sigs
        `(#'assert-same-protocol (var ~name) '~(map :name (vals sigs))))
     ;; (log/debug "DEFPROPS VAR: " (var ~name))
     ;; (log/debug "DEFPROPS OPTS: " '~opts)
     ;; (log/debug "DEFPROPS SIGS: " (str '~sigs))
     (alter-var-root (var ~name)
                     merge
                     (assoc ~opts
                            :html-attrs '~html-attrs
                            :props '~sigs
                            :var (var ~name)
                       ;; :method-map
                       ;;   ~(and (:on opts)
                       ;;         (apply hash-map
                       ;;                (mapcat
                       ;;                 (fn [s]
                       ;;                   [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                       ;;                 (vals sigs))))
                       ;; :method-builders
                       ;;  ~(apply hash-map
                       ;;          (mapcat
                       ;;           (fn [s]
                       ;;             [`(intern *ns* (with-meta '~(:name s) (merge '~s {:protocol (var ~name)})))
                       ;;              #_(emit-method-builder (:on-interface opts) (:name s) (:on s) (:observer s))])
                       ;;           (vals sigs)))
                        ))
    ;; (log/debug "DEFPROPS X")
    ;;  (-reset-methods ~name)
    ;; (log/debug "DEFPROPS " '~name)
     '~name)))

(defmacro defweb-properties
  [name & opts+sigs]
  ;; (log/debug "DEFPROPERTIES: " name opts+sigs)
  (try (emit-properties name opts+sigs)
       (catch Exception e
         (throw (IllegalArgumentException. (str "defproperties " name ": " (.getMessage e)))))))

;; (defn require-foreign
;;   "1. clojure.core/require the ns  2. generate the <link> elts
;;   NB: for clojure.core/require to work the :refer items must be def'd in the ns
;;   so to support jit loading we need to require first w/o :refer, then define the :refers,
;;   then alias as needed"
;;   [& args]
;;   (log/debug "REQUIREing " args)
;;   ;; step 1: clojure.core/require the namespaces, without options
;;   (doseq [arg args]
;;     (let [ns-basic (first arg)
;;           segs (str/split (str ns-basic) #"\.")]
;;          (log/debug "CLOJURE.CORE/REQUIRE: " ns-basic)
;;          (try (clojure.core/require ns-basic :reload)
;;               (catch java.io.FileNotFoundException e
;;                 (throw (Exception.
;;                         (str "miraj.co-dom/require ns undefined: " (.getMessage e))))))
;;          (doseq [[isym ivar] (ns-interns ns-basic)] (log/debug "INTERNED: " isym ivar))
;;          (doseq [[isym ivar] (ns-aliases ns-basic)] (log/debug "ALIAS: " isym ivar))
;;          ;; make sure file actually has ns decl
;;          (if (find-ns ns-basic) nil (throw (Exception. (str "ns not declared: " ns-basic))))

;;          ;; make sure components map is defined
;;          ;; (if (not (ns-resolve ns-basic (symbol "components")))
;;          ;;   (throw (Exception. (str "components map not defined in : " ns-basic))))
;;          ;; step 2: resolve the referenced syms and generate html element fns
;;          (x/resolve-require-refs arg)))

;;   ;; step 3: for each :refer, generate a <link> element
;;   ;; require-resource does both
;;   ;; (log/debug "EXPANDING REQUIRE")
;;   (do
;;      ;; (log/debug "REQUIRing: " [~@args])
;;      (let [link-elts (for [arg args]
;;                         (do ;;(log/debug "GET-REQ: " arg)
;;                           (let [r (x/require-resource arg)]
;;                             (doall r)
;;                             r)))]
;;        (doall link-elts)
;;        ;; (log/debug "REQUIREd: " link-elts#)
;;                           link-elts)))

(defn require-polymer
  [req] ;; e.g.  [polymer.paper :as paper :refer [button card]]
  (log/debug "require-polymer: " req)
  (let [sn (first req)]
    ))

(defn- throw-if
  "Throws a CompilerException with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [^String message (apply format fmt args)
          exception (Exception. message)
          raw-trace (.getStackTrace exception)
          boring? #(not= (.getMethodName ^StackTraceElement %) "doInvoke")
          trace (into-array (drop 2 (drop-while boring? raw-trace)))]
      (.setStackTrace exception trace)
      (throw (clojure.lang.Compiler$CompilerException.
              *file*
              (.deref clojure.lang.Compiler/LINE)
              (.deref clojure.lang.Compiler/COLUMN)
              exception)))))

(defn- refer
  "refers to all public vars of ns, subject to filters.
  filters can include at most one each of:

  :exclude list-of-symbols
  :only list-of-symbols
  :rename map-of-fromsymbol-tosymbol"

  {:added "1.0"}
  [ns-sym & filters]
  ;; (log/debug "refer: " ns-sym filters)
  (let [ns (or (find-ns ns-sym) (throw (new Exception (str "No namespace: " ns-sym))))
        fs (apply hash-map filters)
        nspublics (ns-publics ns)
        rename (or (:rename fs) {})
        exclude (set (:exclude fs))
        to-do (if (= :all (:refer fs))
                (keys nspublics)
                (or (:refer fs) (:only fs) (keys nspublics)))]
    (when (and to-do (not (instance? clojure.lang.Sequential to-do)))
      (throw (new Exception ":only/:refer value must be a sequential collection of symbols")))
    (for [sym to-do]
      (when-not (exclude sym)
        (let [v (nspublics sym)]
          (when-not v
            (throw (new java.lang.IllegalAccessError
                        (if (get (ns-interns ns) sym)
                          (str sym " is not public")
                          (str sym " does not exist")))))
          (do
            ;; (log/debug "REFER " sym v (meta v))
            (. *ns* (refer (or (rename sym) sym) v))
            (:uri (meta v))
            #_(str bower-repo "/" (:uri (meta v)))))))))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defonce ^:dynamic
  ^{:private true
     :doc "A ref to a sorted set of symbols representing loaded libs"}
  *loaded-libs* (ref (sorted-set)))

(defonce ^:dynamic
  ^{:private true :doc
     "True while a verbose load is pending"}
  *loading-verbosely* false)

(defn declare-webcomponent
  [ns-sym nm-sym type & docstring]
  (log/debug "    DECLARE-WEBCOMPONENT:" ns-sym nm-sym docstring) ;; elt-kw uri docstring)
  (let [ds (if (empty? docstring) ""
               (if (string? (first docstring))
                 (first docstring)
                 (if (vector? (first docstring))
                   (if (empty? (last (first docstring))) "" (last (first docstring))))))
        segs (str/split (str ns-sym) #"\.")]
    (if (= (first segs) "polymer")
      (let [polymer-cat (last segs)
            ;; _ (log/debug "polymer category: " polymer-cat)
            html-tag (str polymer-cat "-" nm-sym)
            ;; _ (log/debug "html-tag: " html-tag)
            html-kw (keyword html-tag)
            uri (str bower-repo "/" (if (vector? (first docstring))
                                      (ffirst docstring)
                                      (str html-tag "/" html-tag ".html")))
            ;; _ (log/debug "uri:      " uri)
            newvar (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds :uri uri type true})
                           (fn [& args]
                             (let [elt (if (empty? args)
                                         (do ;; (log/debug "COMPONENT FN NO ARGS: " html-kw)
                                             (x/element html-kw))
                                         (let [first (first args)
                                               rest (rest args)
                                               [attrs content] (x/parse-elt-args first rest)]
                                           (apply x/element html-kw attrs content)))]
                               elt)))]
        ;; (log/debug "NS-SYM: " ns-sym)
        ;; (log/debug "NM-SYM: " nm-sym)
        ;; (log/debug "NEWVAR: " newvar)
        newvar)
      ;; else custom component
      (let [html-tag nm-sym
            html-kw (keyword html-tag)
            uri (str "/" (str/replace (str ns-sym) #"\." "/") "/" nm-sym ".html")
            uri (str/replace uri #"-" "_")
            newvar (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds :uri uri type true})
                           (fn [& args]
                             (let [elt (if (empty? args)
                                         (do ;; (log/debug "COMPONENT FN NO ARGS: " html-kw)
                                             (x/element html-kw))
                                         (let [first (first args)
                                               rest (rest args)
                                               [attrs content] (x/parse-elt-args first rest)]
                                           (apply x/element html-kw attrs content)))]
                               elt)))]
        ;; (log/debug "NS-SYM: " ns-sym)
        ;; (log/debug "NM-SYM: " nm-sym)
        ;; (log/debug "NEWVAR: " newvar)
        newvar))))

;; OBSOLETE: in earlier versions, we dynamically created and populated
;; the lib namespace, rather than loading a lib from a jar.  now we
;; now use statically generated libs in jars
(defn- load-polymer-lib
  "Dynamically load a polymer lib. Create the namespace (e.g. polymer.x); for each
  var y, get config data from the polymer/x map, then intern
  polymer.x/y"
  [lib as-alias require]
  (log/debug "    LOAD-POLYMER-LIB: " lib as-alias require)
  (let [lns (create-ns lib)
        _ (log/debug "    created ns:  " lns)
        segs (str/split (str lib) #"\.")
        pns (find-ns lib) ; 'miraj.polymer)
        _ (log/debug "    polymer ns:  " pns)
        psym (symbol (str lib) (str as-alias))
        _ (log/debug "    psym: " psym)
        v (find-var psym)
        _ (log/debug "    polymer var: " v)
        kw-docstring-map (deref v)]
    (log/debug "var val: " kw-docstring-map)
    (if as-alias
      (do ;; (log/debug "making alias " as-alias lns)
        ;; (log/debug "current ns: " *ns*)
        (alias (symbol as-alias) (symbol lib))
        #_(log/debug "ALIASES: " (ns-aliases *ns*))))
    (doseq [[kw docstring] kw-docstring-map]
      (do
        (let [sym (symbol (name kw))
              elt (declare-webcomponent lns sym :polymer docstring)]
          ;; (log/debug "ELT: " elt)
          #_(load (root-resource lib))
          (throw-if (and as-alias (not (find-ns lib)))
                    "namespace '%s' not found after loading '%s'"
                    lib lib #_(root-resource lib))
          (when require
            (dosync
             (commute *loaded-libs* conj lib)))
          elt)))))

(defn- load-miraj-lib-static-resource
  [ns-sym nm-sym]
  (let [uri (str "/" (str/replace (str ns-sym) #"\." "/") "/" nm-sym ".html")
        uri (str/replace uri #"-" "_")
        _ (log/debug "CHECKING: " uri)
        resource (io/resource uri)]
    (log/debug "RESOURCE: " resource)
    (if (nil? resource) (throw (java.io.FileNotFoundException. (format "web resource %s not found" uri))))
    #_(declare-webcomponent lns sym :_cowebcomponent "")))

(defn- load-miraj-lib
  "load miraj web component lib"
  [lib as-alias require opts]
  (log/debug "    LOAD-MIRAJ-LIB: " lib as-alias require opts)

  ;; in dynamic mode, component is loadable from the defweb-component ns
  ;; in static mode, the component is static html/cljs, not loadable
  ;; so: try to require the lib; if ok then done
  ;; else declare webcomponent var whose metadata is href, not defn

  (let [reqns (try
                (do (clojure.core/require lib)
                    (clojure.core/find-ns lib))
                (catch java.io.FileNotFoundException e
                  (do ;; (log/debug "LOAD_MIRAJ_LIB EXCEPTION" (.getMessage e))
                      nil)))
        lns (if (nil? reqns)
              (clojure.core/create-ns lib)
              reqns)]
    (log/debug "    REQUIRED NS: " reqns)
    (if as-alias
      (alias (symbol as-alias) (symbol lib)))
    ;; TODO: find and load jarfile?
    (doseq [sym (:refer (apply hash-map opts))]
      (let [elt (if (nil? reqns)
                  (load-miraj-lib-static-resource lns sym)
                  (find-var (symbol (str lib) (str sym))))]
        ;; (if (nil? resource) (throw (Exception. (format ":require resource %s not found on classpath" uri))))
        ;; (log/debug "ELT: " elt)
        ;; (log/debug "DEREF ELT: " (deref elt))
        ;; (log/debug "META ELT: " (meta elt))
        ;; (log/debug "ELT URI: " uri)
        ;; (log/debug "ELT RESOURCE: " resource)
        #_(load (root-resource lib))
        (throw-if (and as-alias (not (find-ns lib)))
                  "namespace '%s' not found after loading '%s'"
                  lib lib #_(root-resource lib))
        (when require
          (dosync
           (commute *loaded-libs* conj lib)))
        elt))))

(defn- load-one
  "Loads a lib given its name. If as-alias, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib as-alias require opts]
  (log/debug "    LOAD-ONE: " lib as-alias require opts)
  (log/debug "    ALIAS: " as-alias)
  (log/debug "    OPTS: " opts)
  (let [segs (str/split (str lib) #"\.")
        seg1 (first segs)]
    (log/debug "segs: " segs)
    (if (and (= seg1 "miraj") (= (second segs) "polymer"))
      (do (log/debug "    MIRAJ.POLYMER - requiring")
          #_(let [pl (load-polymer-lib lib as-alias require)]
              pl)
          (let [v [lib :as as-alias]
                args (into v opts)
                args (concat [args] [:verbose])]
            ;;(log/debug "    ARGS: " args)
            (apply clojure.core/require args)
            #_(let [lns (find-ns lib)
                  _ (log/debug "    FOUND ns:  " lns)
                  interns (ns-interns lns)
                  _ (log/debug "    INTERNS:" interns)]))
          )
      (let [cl (load-miraj-lib lib as-alias require opts)]
        ;; (log/debug "custom lib: " cl)
        cl)
      )))

;; (let [lns (create-ns lib)]
;;     (log/debug "created ns: " lns)
;;     (if need-ns
;;       (do ;; (log/debug "making alias " need-ns lns)
;;           ;; (log/debug "current ns: " *ns*)
;;           (alias (symbol need-ns) (symbol lib))
;;           #_(log/debug "ALIASES: " (ns-aliases *ns*)))))
;;   ;; (log/debug "polymer/paper: " polymer/paper)
;;   (log/debug "POLYMER/PAPER: " (find-var (symbol (str/replace (str lib) #"\." "/"))))
;;   (let [tbl (symbol (str/replace (str lib) #"\." "/"))]
;;     (log/debug "tbl: " tbl)
;;     (let [table tbl] ;; (find-var tbl)]
;;       (doseq [sym (eval table)]
;;         (log/debug (str "SYM: " sym)))))
;;   #_(load (root-resource lib))
;;   (throw-if (and need-ns (not (find-ns lib)))
;;             "namespace '%s' not found after loading '%s'"
;;             lib lib #_(root-resource lib))
;;   (when require
;;     (dosync
;;      (commute *loaded-libs* conj lib))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  ;; (log/debug "load-all " lib need-ns require)
  (dosync
   (commute *loaded-libs* #(reduce1 conj %1 %2)
            (binding [*loaded-libs* (ref (sorted-set))]
              (load-one lib need-ns require)
              @*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (log/debug "    LOAD-LIB: " prefix lib options)
  (throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @*loaded-libs* lib)
        ;; _ (log/debug "lib already loaded? " lib loaded)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        as-alias (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (log/debug "    LOAD: " load " as:" as-alias)
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (try
          (load lib as-alias require (mapcat seq filter-opts))
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        #_(throw-if (and as-alias (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      #_(when (and as-alias *loading-verbosely*)
        (printf "    (clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      #_(when as
        (when *loading-verbosely*
          (printf "    (clojure.core/alias '%s '%s)\n" as-alias lib))
        (alias as lib))
      (when (or use (:refer filter-opts))
        #_(when *loading-verbosely*
          (printf "    (miraj.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf "    KEYOPT: %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (let [result (apply refer lib (mapcat seq filter-opts))]
          #_(log/debug "    REFER RESULT: " result)
          result)))))

(defn- load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  (log/debug "    LOAD-LIBS " args)
  ;; step 1: clojure.core/require the namespaces, without options
  ;;(doseq [arg args]
    (let [;; ns-basic (first arg)
          ;; segs (str/split (str ns-basic) #"\.")
          flags (filter keyword? args)
          opts (interleave flags (repeat true))
          args (filter (complement keyword?) args)]
      ;; (log/debug "MIRAJ.CORE/REQUIRE: " #_ns-basic)
      ;; (log/debug "flags: " flags)
      ;; (log/debug "opts:  " opts)
      ;; (log/debug "args:  " args)
      (let [supported #{:as :reload :reload-all :verbose :refer :require}
            unsupported (seq (remove supported flags))]
        (throw-if unsupported
                  (apply str "Unsupported option(s) supplied: "
                         (interpose \, unsupported))))
    ; check a load target was specified
    (throw-if (not (seq args)) "Nothing specified to load")
    (for [arg args]
      (if (libspec? arg)
        (apply load-lib nil (prependss arg opts))
        (let [[prefix & args] arg]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (prependss arg opts))))))))

      ;; #_(if (= (first segs) "polymer")
      ;;   (require-polymer arg))))

;; CHANGED: we used to insert polymer link elts at defpage time; now
;; we do it in the normalize routine.
(defn require
  "Called by defpage to have clojure.core/require load polymer libs."
  [page-var & args]
  ;; (log/debug "    :REQUIRE " page-var args)
  (let [reqres (remove nil? (flatten (apply load-libs :require args)))
        ;; _ (log/debug "    REQRESULT: " reqres)
        ;; reqelts (for [arg args] ;; [req reqres]
        ;;           (do ;; (log/debug "arg: " arg)
        ;;           (x/element :link {:rel "import"
        ;;                             :href (str (first arg))
        ;;                             #_req})))
        ]
    ;; (log/debug "    :REQUIRE RESULT: " reqelts)
    ;;(alter-meta! page-var (fn [old] (assoc old :_webcomponents args)))
    [:require nil #_reqelts]))

;;obsolete
#_(defn import-resources
  [resources imports-map]
  ;; (log/debug "import-resource: " resources imports-map)
  (let [ns (first resources)
        ;; _ (log/debug "ns: " ns)
        resources (next resources)
        ;; _ (log/debug "resources: " resources)
        ]
    ))

;;FIXME: put this in webc?
(defn get-imports-config-map
  []
  (let [home-imports-path (str (System/getProperty "user.home") "/.miraj/imports.edn")
        ;; _ (log/debug "home-imports-path: " home-imports-path)
        home-imports (try (slurp home-imports-path)
                          (catch java.io.FileNotFoundException e nil))
        home-imports (if home-imports (read-string home-imports) nil)
        ;; _ (log/debug "HOME IMPORTS.EDN: " home-imports)
        proj-imports (try (slurp ".miraj/imports.edn")
                          (catch java.io.FileNotFoundException e nil))
        proj-imports (if proj-imports (read-string proj-imports) nil)
        ;; _ (log/debug "PROJ IMPORTS.EDN: " proj-imports)
        ]
    (merge-with merge home-imports proj-imports)))

(defn css-spec->link
  [spec]
  ;; (log/debug "css-spec->link: " spec)
  (let [css-ns   (first spec)
        css-vars (rest  spec)]
    (for [css-var css-vars]
      (let [path (ns-sym->path css-ns)
            href (str "/" path "/" css-var ".css")]
        ;;(log/debug "IMPORT HREF: " href)
        (x/element :link {:rel "stylesheet" :href href})))))

(defn css-map->link
  [css-map]
  ;; (log/debug "css-map->link: " css-map)
  (let [supported (set (conj (keys x/html5-link-attrs))) ;; html5-link-types
        unsupported (seq (remove supported (keys css-map)))]
    (throw-if unsupported
              (apply str "Unsupported :css option(s) supplied: "
                     (interpose \, unsupported))))
  (x/element :link (conj {:rel "stylesheet" :type "text/css"}
                         css-map)))

(defn css
  "Fn that handles :css directive of miraj.core/defpage"
  [page-var & css-specs]
  (log/debug "CSS: " page-var) ;;  css-specs)
  (let [flags (filter keyword? css-specs)
        opts (apply hash-map (interleave flags (repeat true)))
        args (filter (complement keyword?) css-specs)]
    ;; check for unsupported options
    (let [supported (set (conj (keys x/html5-link-attrs) :custom)) ;; html5-link-types
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported :css option(s) supplied: "
                       (interpose \, unsupported))))
    (let [arg1 (first args)
          elts (cond
                  (:custom opts)
                  (let [css (second args)]
                    ;; FIXME: this assumes css is a string
                    (x/element :style {:is "custom-style" :type "text/css"} args))

                  (string? arg1) (x/element :style {:type "text/css"} arg1)

                  (vector? arg1) ;; maps and import vectors
                  (for [css-spec arg1]
                    (vector (if (vector? css-spec)
                              (css-spec->link css-spec)
                              (if (map? css-spec)
                                (css-map->link css-spec)
                                (throw (Exception. (format "Invalid :css vector: %s" css-spec)))))))

                  :else
                  (for [css-spec args]
                    (css-spec->link css-spec))
                  )]
      ;;(log/debug "css elts: " elts)
      [:css elts])))

(defn js
  "Fn that handles :js directive of miraj.core/defpage"
  [page-var & css-specs]
  (log/debug "JS: " page-var css-specs)
  (let [flags (filter keyword? css-specs)
        opts (apply hash-map (interleave flags (repeat true)))
        args (filter (complement keyword?) css-specs)]
    ;; (log/debug "ARGS: " args)
    ;; check for unsupported options
    (let [supported x/html5-script-attrs
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported :css option(s) supplied: "
                       (interpose \, unsupported))))
    (let [arg1 (first args)
          elts (cond

                  ;; (:custom opts)
                  ;; (let [css (second args)]
                  ;;   ;; FIXME: this assumes css is a string
                  ;;   (x/element :style {:is "custom-style" :type "text/css"} args))

                  (string? arg1) (x/element :script {:type "text/javascript"} arg1)

                  (vector? arg1) ;; maps and import vectors
                  (for [css-spec arg1]
                    (vector (if (vector? css-spec)
                              (css-spec->link css-spec)
                              (if (map? css-spec)
                                (css-map->link css-spec)
                                (throw (Exception. (format "Invalid :css vector: %s" css-spec)))))))

                  :else
                  (for [css-spec args]
                    (css-spec->link css-spec))
                  )]
      ;;(log/debug "css elts: " elts)
      [:css elts])))

(defn import
  ""
  [page-var & import-specs]
  (log/debug "IMPORT: " page-var import-specs)
  (let [flags (filter keyword? import-specs)
        opts (apply hash-map (interleave flags (repeat true)))
        args (first (filter (complement keyword?) import-specs))]
    ;; (log/debug "Args: " args)
    ;; (log/debug "OPTS: " opts)
    ;; check for unsupported options
    (let [supported #{:modules :custom :uri}
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported :import option(s) supplied: "
                       (interpose \, unsupported))))
    (let [imports (for [import-spec args]
                    (let [import-ns   (first import-spec)
                          import-vars (rest import-spec)
                          path (if (:uri opts) (ns->uri import-ns) (ns-sym->path import-ns))]
                      ;; (log/debug "PATH: " path)
                      (if (:modules opts)
                        (vector (x/element :link {:rel "import"
                                                  :href (str "/" path ".html")})
                                (for [import-var import-vars]
                                    (x/element :style
                                               (merge {:include (str import-var)}
                                                      (if (:custom opts)
                                                        {:is "custom-style"}
                                                        {})))))

                        (for [import-var import-vars]
                          (let [href (str "/" path "/" import-var ".html")]
                            (x/element :link {:rel "import" :href href}))))))]
      ;; (log/debug "imports: " imports)
      [:import imports])))

#_(defn import-x
  ""
  [page-var & args]
  (log/debug "IMPORT-X: " page-var args)
  (let [imports-config-map (get-imports-config-map)
        ;; _ (log/debug "IMPORTS CONFIG MAP: " imports-config-map)
        imports (for [arg args]
                    (let [ns-basic (first arg)
                          resources (next arg)]
                      (for [resource resources]
                        (let [r (get-in imports-config-map [ns-basic resource])]
                          (if (nil? r) (throw (Exception. (format "resource %s/%s not configured."
                                                                  ns-basic resource))))
                          (if (nil? (:type r)) (throw (Exception. (format "resource %s/%s not typed."
                                                                          ns-basic resource))))
                          ;;FIXME: validate: one of :file or :cdn must be present
                          (if-let [cdn (:cdn r)]
                            (if (= (:type r) :js)
                              (x/element :script {:src cdn :type "text/javascript"})
                              (x/element :link {:href cdn :rel "stylesheet"}))
                            (if (= (:type r) :js)
                              (x/element :script {:src (:file r) :type "text/javascript"})
                              (x/element :link {:href (:file r) :rel "stylesheet"})))))))]
                        ;; #_(import-resources resources (get imports-config-map ns-basic)))))]
    ;; (log/debug "IMPORTS: " imports)
    (alter-meta! page-var (fn [old] (assoc old :_webimports args)))
    [:import imports]))

      ;; (if (= (first segs) "polymer")
      ;;   (require-polymer arg)))))

  ;;     (try (clojure.core/require ns-basic :reload)
  ;;             (catch java.io.FileNotFoundException e
  ;;               (throw (Exception.
  ;;                       (str "miraj.co-dom/require ns undefined: " (.getMessage e))))))
  ;;        (doseq [[isym ivar] (ns-interns ns-basic)] (log/debug "INTERNED: " isym ivar))
  ;;        (doseq [[isym ivar] (ns-aliases ns-basic)] (log/debug "ALIAS: " isym ivar))
  ;;        ;; make sure file actually has ns decl
  ;;        (if (find-ns ns-basic) nil (throw (Exception. (str "ns not declared: " ns-basic))))

  ;;        ;; make sure components map is defined
  ;;        ;; (if (not (ns-resolve ns-basic (symbol "components")))
  ;;        ;;   (throw (Exception. (str "components map not defined in : " ns-basic))))
  ;;        ;; step 2: resolve the referenced syms and generate html element fns
  ;;        (x/resolve-require-refs arg)))

  ;; ;; step 3: for each :refer, generate a <link> element
  ;; ;; require-resource does both
  ;; ;; (log/debug "EXPANDING REQUIRE")
  ;; (do
  ;;    ;; (log/debug "REQUIRing: " [~@args])
  ;;    (let [link-elts (for [arg args]
  ;;                       (do ;;(log/debug "GET-REQ: " arg)
  ;;                         (let [r (x/require-resource arg)]
  ;;                           (doall r)
  ;;                           r)))]
  ;;      (doall link-elts)
  ;;      ;; (log/debug "REQUIREd: " link-elts#)
  ;;                         link-elts)))

(defn body
  ""
  [page-var & args]
  ;; (log/debug "BODY: " page-var)
  ;; (log/debug "BODY ARGS: " args)
  (let [page-ns (-> page-var meta :ns)
        page-ns-sym (ns-name page-ns)
        ;;_ (clojure.core/require page-ns-sym :reload) ; :verbose)
        content (map #(eval %) args)]
    ;; force eval of content, so namespace aliases are resolvable!
    (doall content)
    [:body content]))

;;OBSOLETE
;; #_(defmacro co-fn
;;   [fn-tag docstring elt-kw elt-uri typ]
;;   (do #_(log/debug "co-fn:" typ fn-tag elt-kw elt-uri docstring)
;;               (eval `(defn ~fn-tag ~docstring
;;                        [& args#]
;; ;;                       (log/debug "invoking " ~fn-tag)
;;                        (let [elt# (if (empty? args#)
;;                                     (with-meta (element ~elt-kw)
;;                                       {:miraj
;;                                        {:co-fn true
;;                                         :component ~typ
;;                                         :doc ~docstring
;;                                         :elt-kw ~elt-kw
;;                                         :elt-uri ~elt-uri}})
;;                                     (let [first# (first args#)
;;                                           rest# (rest args#)
;;                                           [attrs# content#] (parse-elt-args first# rest#)]
;;                                       (with-meta (apply element ~elt-kw attrs# content#)
;;                                         {:miraj/miraj {:co-fn true
;;                                                  :component ~typ
;;                                                  :doc ~docstring
;;                                                  :elt-kw ~elt-kw
;;                                                  :elt-uri ~elt-uri}})))]
;;                          elt#)))
;;               (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
;;                             (fn [old new]
;;                               (merge old new))
;;                             {:miraj/miraj {:co-fn true
;;                                      :component typ
;;                                      :doc docstring
;;                                      :elt-kw elt-kw
;;                                      :elt-uri elt-uri}})))

(defn codom
  "called by defweb-codom processing"
  [page-var & args]
  (let [content (map #(eval %) args)]
        ;;bod (x/element :body content)]
    ;; (log/debug ":BODY " content)
    [:miraj/codom content]))

(defmacro defweb-codom
  ""
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  ;; (log/debug "DEFWEB-CODOM " name " in ns " *ns*)
  (let [codom-var (intern *ns* name)
        ;; _ (log/debug "PAGE VAR: " codom-var)
        process-reference
        (fn [[kname & args]]
          `(do
             ;; (log/debug "PROCESSING: " '~(symbol "miraj.core" (clojure.core/name kname)))
             ;; (log/debug "ARGS: " '~args)
             (~(symbol "miraj.core" (clojure.core/name kname))
              ~codom-var
              ~@(map #(list 'quote %) args))))
        docstring  (when (string? (first references)) (first references))
        ;; _ (log/debug "DOCSTRING: " docstring)
        [args references] (if docstring
                            [(fnext references) (nnext references)]
                            [(first references) (next references)])
        ;; _ (log/debug "ARGS: " args)
        ;; references (if docstring (next references) references)
        ;; _ (log/debug "REFS: " references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        ;; _ (log/debug "NAME: " name (meta name))
        metadata {}
        ;; metadata (eval `(let [maybe-meta#  ~(first references)]
        ;;            (cond (map? maybe-meta#) maybe-meta#
        ;;                  (symbol? maybe-meta#) (if (map? maybe-meta#) maybe-meta#))))
        ;; ;; _ (log/debug "metadata: " metadata)
        ;; references (if metadata (next references) references)
        ;; name (if metadata
        ;;        (vary-meta name merge metadata)
        ;;        name)
        name-metadata (meta name)]
    `(do
       (with-loading-context
         ;; (let [[reqs# imports# body#] [~@(map process-reference references)]
         ;;       head# (x/element :head reqs# imports#)
         ;;       html# (x/element :html head# body#)]
         (let [reqs# (into {} [~@(map process-reference references)])
               ;; head# (apply x/element :head {} (vec (flatten (list (:require reqs#) (:import reqs#)))))
               head# (flatten (list (:require reqs#) (:import reqs#)))
               ;; _# (log/debug "HEAD# " head#)
               ;; body# (apply x/element :codom {} (:codom reqs#))
               body# (:miraj/codom reqs#)
               ;; html# (apply x/element :html {} (vec (flatten (list head# body#))))
               codom# (concat head# body#)

               ;; _# (log/debug "ALL: " reqs#)
               ;; _# (log/debug "HEAD# " head#)
               ;; _# (log/debug "BODY# " body#)
               ;; _# (log/debug "CODOM# " codom#)

           ;; (log/debug "HTML# " html#)
           ;; (clojure.core/intern *ns* '~name html#)
           ;; (intern *ns*
           ;;         (with-meta
           ;;           (symbol ~(str name))
           ;;           (merge {:doc ~docstring :_webpage true} ~metadata)))

               tree# (apply x/element ;;~(keyword nm)
                            :CODOM_56477342333109
                            {:id ~(str name)} codom#)
               codom-norm# (x/xsl-xform x/xsl-normalize-codom tree#)]
           (clojure.core/alter-var-root ~codom-var
                                        (fn [old# & args#] codom-norm#))
           (clojure.core/alter-meta! ~codom-var
                                     (fn [old# & args#]
                                       (merge old# ~metadata {:doc ~docstring :miraj/codom true})))
           ~codom-var)))))

         ;; (intern *ns* (with-meta (symbol ~(str nm-sym)) {:doc ~docstr :codom true})
         ;;         codom#)
         ;; codom#))))


(defn props->cljs
  [propmap]
  ;; (log/debug "PROPS->CLJS: " propmap)
  ;; (if (not (:props propmap))
  ;;   (throw (IllegalArgumentException. (str "props->cljs arg must be a Properties map"))))
  (let [props (:properties propmap)
        prop-keys (keys props)
        html-attrs (:html-attrs propmap)]
    ;; (log/debug (str (:on propmap) ": " prop-keys))
    {:hostAttributes html-attrs
     :properties (into {}
                       (for [prop-key prop-keys]
                         (let [prop (get props prop-key)
                               ;; _ (log/debug "PROP: " prop)
                               descriptors (keys prop)
                               type+val (merge {:type (cljtype->jstype (:type prop))}
                                               (if (= 'String (:type prop))
                                                 (if (nil? (:value prop))
                                                   {}
                                                   (if (empty? (:value prop))
                                                     {:value "\"\""}
                                                     {:value (:value prop)}))
                                                 (if (not (nil? (:value prop)))
                                                   {:value (:value prop)}
                                                   {})))
                               flags (into {} (for [flag (:flags prop)]
                                                [(cljkey->jskey flag) true]))
                               ;; _ (log/debug "FLAGS: " flags)
                               ]
                           ;; (log/debug "procesing property: " (pr-str prop))
                           ;; (log/debug "descriptors: " descriptors)
                           {(keyword (:name prop)) (merge type+val flags
                                                          (if (:observer prop)
                                                            {:observer (first (:observer prop))}))})))}))

(defn listeners->cljs
  [ls]
  ;; (log/debug "LISTENERS->CLJS: " (:listeners ls))
  {:listeners
   (into {}
         (merge
          (concat
           (flatten (for [[evsuite listeners] (:listeners ls)]
                      (do ;;(log/debug "EVENT SUITE: " evsuite)
                        (for [[ev listener] listeners]
                          (do ;;(log/debug "EV/LISTENER: " ev "/" listener)
                            {ev (keyword (first listener))}
                            ))))))))
         })

(defn methods->cljs
  [ms]
  ;; (log/debug "METHODS->CLJS: " ms)
  (let [observers (map #(:observer %)
                       (filter #(:observer %) (vals (:properties ms))))
        ;; _ (log/debug "OBSERVERS: " observers)
        ls (vals (reduce merge (vals (:listeners ms))))
        ;; _ (log/debug "LISTENER METHODS: " ls)
        behs (vals (reduce merge (vals (:behaviors ms))))
        ;; _ (log/debug "BEH METHODS: " behs)
        this-methods (vals (reduce merge (vals (:methods ms))))
        ;; _ (log/debug "THIS METHODS: " this-methods)
        methods (concat observers ls behs this-methods)
        ;; _ (log/debug "METHODS: " methods)
        ]
    (into {} methods)))

  ;; {:methods
  ;;  (into {}
  ;;        (merge
  ;;         (concat
  ;;          (flatten (for [[evsuite listeners] (:listeners ms)]
  ;;                     (do ;;(log/debug "EVENT SUITE: " evsuite)
  ;;                       (for [[ev listener] listeners]
  ;;                         (do ;;(log/debug "EV/LISTENER: " ev "/" listener)
  ;;                           {ev (keyword (first listener))}
  ;;                           ))))))))
  ;;        })

(defn behaviors->cljs
  [behs]
  ;; (log/debug "BEHAVIORS->CLJS: " (:behaviors behs))
  (let [behaviors (keys  (:behaviors behs))]
    {:behaviors (into [] (map keyword behaviors))}))

(defn component->prototype
  [cvar html-tag rawprops rawlisteners rawbehaviors rawmethods]
  ;; (log/debug "COMPONENT->PROTOTYPE: " cvar (type cvar))
  ;; (log/debug "RAWLISTENERS: " rawlisteners)
  ;; (log/debug "RAWMETHODS: " rawmethods)
  ;; (log/debug "RAWBEHAVIORS: " rawbehaviors)
  (let [;; v (resolve nm)
        ;; _ (log/debug "component var: " cvar)
        ;; _ (log/debug "props: " props)
        ;; namesp (:ns (meta cvar))
        ;; ns-name (ns-name namesp)
        uri (str "tmp/" (var->path cvar) ".cljs")
        cljs-ns (symbol (str (-> cvar meta :ns ns-name) "." html-tag)) ;; ".core"))
        ;; cljs-ns (symbol (str (var->cljs-ns cvar) ".core"))
        propmap (props->cljs rawprops)
        listeners (listeners->cljs rawlisteners)
        methmap (methods->cljs (merge rawprops rawlisteners rawbehaviors rawmethods))
        ;; _ (log/debug "METHODS MAP: " methmap)
        behaviors (behaviors->cljs rawbehaviors)
        ;; cljs (str/join "\n" [(pprint-str (list 'ns cljs-ns))
        ;;                      (pprint-str '(enable-console-print!))
        ;;                      (pprint-str '(log/debug "hello"))])
        ;;                      ;; (pprint-str (list 'js/Polymer
        ;;                      ;;                   (list 'clj->js
        ;;                      ;;                         (merge {:is (keyword (:name (meta cvar)))}
        ;;                      ;;                                propmap
        ;;                      ;;                                listeners
        ;;                      ;;                                methmap
        ;;                      ;;                                behaviors))))])

        ;; FIXME: parameterize repl and console print

        cljs (str/join "\n" [(pprint-str (list 'ns cljs-ns
                                               #_'(:require #_[clojure.browser.repl :as repl]
                                                          #_[weasel.repl :as repl])))
                             ;; FIXME: parameterize host and port
                             #_(pprint-str '(when-not (repl/alive?)
                                            (repl/connect "ws://localhost:9001/repl")))
                             #_(pprint-str '(defonce conn
                                            (repl/connect "http://localhost:9000/repl")))
                             (pprint-str (list 'enable-console-print!))
                             (pprint-str (list 'js/Polymer
                                               (list 'clj->js
                                                     (merge {:is (keyword html-tag)}; (keyword (:name (meta cvar)))}
                                                            propmap
                                                            listeners
                                                            methmap
                                                            behaviors))))])
        ]
    ;; (log/debug "CLJS:\n" cljs)
    ;; (log/debug "URI: " uri)
    ;; (io/make-parents uri)
    ;; (spit uri cljs)
    (alter-meta! cvar
               (fn [old new]
                 (merge old new))
               {:miraj/miraj {:miraj/prototype cljs}})))

(defmacro defcomponent
  "Define a web component (Polymer flavor)"
  [[nm html-tag] & args]
  ;; (log/debug "DEFCOMPONENT: " (str html-tag)) ;; " ARGS: " args)
  (if (not (str/includes? html-tag "-")) (throw (IllegalArgumentException.
                                           (str "Component name must contain at least one dash '-'."))))
  ;; (log/debug "COMPONENT ARGS: " (pr-str args))
  (let [[docstr arglist codom & protos] (parse-cotype-args args)
        ;; _ (log/debug "DOCSTR: " (pr-str docstr))
        ;; _ (log/debug "ARGLIST: " (pr-str arglist))
        ;; _ (log/debug "COTYPE PROTOS: " protos (seq protos))
        ;; _ (log/debug "CODOM: " codom)
        rawprops (props->propmap args)

        rawmethods (apply protos->rawmethods protos)

        rawbehaviors (apply protos->rawbehaviors protos)

        rawlisteners (apply protos->rawlisteners protos)

        ;; codom (drop 1 cod)
        behavior-elts (behaviors->elements protos)
        ;; _ (log/debug (str "BEHAVIOR-ELTS: " (pr-str behavior-elts)))
        ;; protomap (->protomap protos)
        ;; _ (log/debug "PROTOMAP: " protomap)

        ;; js-ctor (js-constructor html-tag arglist protos)
        ;; js-ctor (element :script (str *ns* "." html-tag "()"))

        base-path (str (ns->path *ns*) "/" html-tag)
        ;; _ (log/debug "DEFC BASE-PATH: " base-path)
        ]
    `(let [tree# ~@codom
       ;; html-constructor interns the name, binding it to ->html fn
           cvar# (x/html-constructor ~*ns* '~nm (keyword '~html-tag) (str (ns->uri ~*ns*) "/" '~html-tag)
                                     ~docstr)
           _# (apply component->prototype cvar# '~html-tag '~rawprops '~rawlisteners '~rawbehaviors '~rawmethods)
           ;; _# (log/debug "PROTOTYPE: " (:prototype (:miraj/miraj (meta cvar#))))
             ;; (if (instance? miraj.co-dom.Element ~(first codom))
             ;;         ~@codom
             ;;         (codom ~html-tag ~@codom))
             content# (:content tree#)
             result# (update tree#
                            :content (fn [c#]
                                       (let [dom# (last c#)
                                             ;; _# (log/debug "DOM#: " dom#)
                                             newdom# (update dom#
                                                             :attrs
                                                             (fn [old-attrs#]
                                                               (assoc old-attrs# :id (str '~html-tag))))
                                             ;; newdom# (update newdom#
                                             ;;                 :content (fn [domc#]
                                             ;;                            (concat domc# [~js-ctor])))
                                             ]
                                         ;; (log/debug "NEWDOM#: " newdom#)
                                         (concat (butlast c#)
                                                 [~@behavior-elts]
                                                 #_[dom#]
                                                 [newdom#]
                                                 #_[(x/element :link {:rel "import"
                                                                    :base-path (str '~html-tag ".js")})]
                                                 ))))
             ;; var# (find-var (symbol (str ~*ns*) (str '~html-tag)))
           ]
         ;; (println "ALTERING META FOR " cvar#)
         (alter-meta! cvar#
                      (fn [old# new#]
                        (do ;;(println (format "old#: %s" old#))
                            ;;(println "new#: " new#)
                            ;; (merge
                             #_(update-in old# [:doc] (:doc new#))
                             ;; (update-in old# [:miraj/miraj]
                             ;;            (merge (:miraj-miraj old#)
                             ;;                   (:miraj-miraj new#))))))
                            (merge old#
                                   {:miraj/miraj (merge (:miraj/miraj old#)
                                                        new#)})))  ;;)
                        {:miraj/component true
                         :miraj/html-tag '~html-tag
                         :miraj/ns (str *ns* "." '~html-tag)
                         :miraj/assets {:miraj/base ~base-path}
                         :miraj/codom result#}  ;; compile serializes codom to an html file
                       ;;:doc ~(str docstr)
                       )
         ;(println "ALTERED Meta for " cvar# (meta cvar#))
         cvar#)))

(defmacro deflibrary
  "Define a webcomponent library"
  {:arglists '([name docstring? attr-map? args*])
   :added "1.0"}
  [name & args]
  ;; (log/debug "DEFLIBRARY: " name " in " *ns*)
  (let [lib-sym (symbol (str (-> *ns* ns-name)) (str name))
        docstring  (when (string? (first args)) (first args))
        ;; _ (println "DOCSTR: " docstring)
        args (if docstring (fnext args) (first args))
        ;; _  (println "ARGS: " args)
        ;; miraj-meta {:miraj/miraj (merge {:miraj/deflibrary true}
        ;;                                 (if (:miraj/defelements args)
        ;;                                   {:miraj/defelements true}
        ;;                                   (if (:miraj/defstyles args)
        ;;                                     {:miraj/defstyles true}
        ;;                                     {})))}

        _ (if (not (or (:miraj/defelements args)
                       (:miraj/defstyles args)))
            (throw (Exception. "deflibrary map must have entry :defelements true or :defstyles true")))

        miraj-meta {:miraj/miraj {:miraj/deflibrary
                                  (if (:miraj/defelements args)
                                    :miraj/elements
                                    (if (:miraj/defstyles args)
                                      :miraj/styles))}}
        ;; _ (println "miraj-meta " miraj-meta)
        ]
  `(do (if ~docstring
         (def ~lib-sym ~(str docstring) ~args)
         (def ~lib-sym ~args))
       ;; set metadata for var
       (clojure.core/alter-meta! (-> ~lib-sym var)
                                 (fn [old# & args#] (merge old# ~miraj-meta))
                              )
       ;; set metadata for enclosing ns (not the var of the ns)
       (alter-meta! *ns* (fn [old#] (merge old# {:miraj/miraj {:miraj/deflibrary true}}))))))

;; defpage, based on clojure.core/defn?
(defmacro defpage
  "define a web page"
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  (log/debug "DEFPAGE: " name " in " *ns*)
  ;; (log/debug "    REFS: " references)
  (let [page-var (intern *ns* name)
        ;; _ (log/debug "PAGE VAR: " page-var)
        ;; process-reference will call fn require for :require, fn import for :import, etc.
        process-reference
        (fn [[kname & args]]
          `(do
             ;; (log/debug "    PROCESSING: " '~(symbol "miraj.core" (clojure.core/name kname)))
             ;; (log/debug "    ARGS: " '~args)
             (~(symbol "miraj.core" (clojure.core/name kname))
              ~page-var
              ~@(map #(list 'quote %) args))))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata (eval `(let [maybe-meta#  ~(first references)]
                   (cond (map? maybe-meta#) maybe-meta#
                         (symbol? maybe-meta#) (if (map? maybe-meta#) maybe-meta#))))
        ;; _ (log/debug "    METADATA: " metadata)
        references (if metadata (next references) references)
        ;; _ (log/debug "    REFERENCES: " references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        ;; _ (log/debug "defweb-page NAME: " name)
        ;; gen-class-clause (first (filter #(= :gen-class (first %)) references))
        ;; gen-class-call
        ;;   (when gen-class-clause
        ;;     (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
        ;; references (remove #(= :gen-class (first %)) references)
        ;ns-effect (clojure.core/in-ns name)
        name-metadata (meta name)]
    `(do
       (with-loading-context
         ;; (let [[reqs# imports# body#] [~@(map process-reference references)]
         ;;       head# (x/element :head reqs# imports#)
         ;;       html# (x/element :html head# body#)]
         (let [reqsvec# [~@(map process-reference references)]
               ;; _# (log/info "reqsvec#: " reqsvec#)
               foo# (for [v# reqsvec#] {(first v#) [(second v#)]})
               ;; _# (log/info "foo#: " foo#)
               reqs# (apply merge-with concat foo#)
               ;; reqs# (into {} reqsvec#)
               ;; _# (log/info "reqs#: " reqs#)
               head# (apply x/element :head {} (vec (flatten (list
                                                              (:require reqs#)
                                                              (:import reqs#)
                                                              (:css reqs#)))))
               ;; _# (log/debug "HEAD# " head#)
               body# (apply x/element :body {} (doall (:body reqs#)))
               html# (binding [*ns* ~*ns*]
                       (apply x/element :html {} (vec (flatten (list head# body#)))))
               ]
           ;; (log/debug "ALL: " reqs#)

           ;; (log/debug "BODY# " body#)
           ;; (log/debug "HTML# " html#)
           ;; (clojure.core/intern *ns* '~name html#)
           ;; (intern *ns*
           ;;         (with-meta
           ;;           (symbol ~(str name))
           ;;           (merge {:doc ~docstring :_webpage true} ~metadata)))
           (clojure.core/alter-var-root ~page-var
                                        (fn [old# & args#] html#))
           (clojure.core/alter-meta! ~page-var
                                     (fn [old# new#]
                                       #_(merge old#
                                              new#
                                              ~metadata
                                              {:doc ~docstring :_webpage true})
                                       (merge old#
                                              {:doc ~docstring}
                                              {:miraj/miraj (merge (:miraj/miraj old#)
                                                                   ~metadata
                                                                   new#)}))
                                     {:miraj/defpage true}
                                     )
           (alter-meta! *ns* (fn [old#] (merge old# {:miraj/miraj {:miraj/defpage true}})))
           ~page-var)))))

;; #_(defn- get-components
;;   []
;;   (log/debug "TASK: boot-miraj/extract")
;;   (let [root-dir (or root-output-dir "miraj")
;;         tmp-dir (io/file (core/tmp-dir!) root-dir)
;;         tmp-output-path  (.getPath tmp-dir)
;;         ;; _ (log/debug "tmp-dir: " tmp-dir)
;;         html-output-dir (if html-output-dir (io/file tmp-dir html-output-dir) tmp-dir)
;;         html-output-path  (.getPath html-output-dir)
;;         cljs-output-dir   (if cljs-output-dir (io/file tmp-dir cljs-output-dir) tmp-dir)
;;         cljs-output-path  (.getPath cljs-output-dir)
;;         last-fileset (atom nil)
;;         ;; pod          (-> (core/get-env)
;;         ;;                  (update-in [:dependencies] into '[[miraj/co-dom "0.1.0-SNAPSHOT"]])
;;         ;;                  pod/make-pod  ;; use pod-pool?
;;         ;;                  future)
;;         ]
;;     (core/with-pre-wrap fileset
;;       (pod/with-eval-in @pod
;;         (require '[miraj.co-dom :as miraj])
;;         (do
;;           (if ~ns-str
;;             (let [ns-sym (symbol ~ns-str)]
;;               (require ns-sym)
;;               (let [interns (ns-interns ns-sym)
;;                     bld (resolve 'miraj.co-dom/build-component)]
;;                 (doseq [[isym ivar] interns]
;;                   (if (:webcomponent (meta ivar))
;;                     (miraj.co-dom/build-component [~html-output-path ~cljs-output-path]
;;                                                   [isym ivar]))))))
;;           (if ~component
;;             (do
;;               (boot.util/info "processing var: " ~component "\n")
;;               (let [widget (symbol ~component)
;;                     ns-sym (symbol (namespace widget))
;;                     html-tag (symbol (name widget))]
;;                 (require ns-sym)
;;                 (let [ivar (resolve widget)]
;;                   (if (:webcomponent (meta ivar))
;;                     (miraj/build-component [~html-output-path ~cljs-output-path]
;;                                            [html-tag ivar]))))))))
;;       (core/sync! root-dir tmp-dir)
;;       fileset)))

(defn make-resource-fns
  [typ tags]
  (do ;;(println "make-resource-fns: " typ tags)
        (doseq [[fn-tag elt-kw elt-uri docstring] tags]
          (do #_(println "make resource:" fn-tag elt-kw elt-uri docstring)
              (eval `(defn ~fn-tag ~docstring
                       [& args#]
;;                       (println "invoking " ~fn-tag)
                       (let [elt# (if (empty? args#)
                                    (with-meta (x/element ~elt-kw)
                                      {:miraj/miraj
                                       {:co-fn true
                                        :component ~typ
                                        :doc ~docstring
                                        :elt-kw ~elt-kw
                                        :elt-uri ~elt-uri}})

                                    ;; (let [attrib-args# (first args#)
                                    ;;       attrs# (if (map? attrib-args#)
                                    ;;                (do ;(log/trace "map? first")
                                    ;;                  (if (instance? miraj.co-dom.Element attrib-args#)
                                    ;;                    (do ;(log/trace "Element instance")
                                    ;;                      {})
                                    ;;                    (do ;(log/trace "NOT Element instance")
                                    ;;                      attrib-args#)))
                                    ;;                (do ;(log/trace "NOT map? attrib-args#")
                                    ;;                  {}))
                                    ;;       content# (if (map? attrib-args#)
                                    ;;                  (if (instance? miraj.co-dom.Element attrib-args#)
                                    ;;                    args#
                                    ;;                    (rest args#))
                                    ;;                  args#)]
                                    (let [first# (first args#)
                                          rest# (rest args#)
                                          [attrs# content#] (x/parse-elt-args first# rest#)]
                                      (with-meta (apply x/element ~elt-kw attrs# content#)
                                        {:miraj/miraj {:co-fn true
                                                 :component ~typ
                                                 :doc ~docstring
                                                 :elt-kw ~elt-kw
                                                 :elt-uri ~elt-uri}})))]
                         elt#)))
              (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
                            (fn [old new]
                              (merge old new))
                            {:miraj/miraj {:co-fn true
                                     :component typ
                                     :doc docstring
                                     :elt-kw elt-kw
                                     :elt-uri elt-uri}})
              #_(println "var: " (find-var (symbol (str *ns*) (str fn-tag))))))))

;;(log/debug "loaded miraj/core.clj")
