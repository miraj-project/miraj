(ns ^{:doc "Miraj core functions"
      :author "Gregg Reynolds"}
  miraj.core
  (:refer-clojure :exclude [import refer require])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clj-time.core :as t]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [boot.core :as boot]
            [boot.pod :as pod]
            [boot.util :as util]
            [boot-bowdlerize :as bow]
            [miraj.polymer :as polymer :refer :all]
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

(println "loading miraj/core.clj")

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

(defn ns->uri [n] (str/replace n #"\." "/"))

(defn ns->path [namesp]
  (println "ns->path: " namesp)
  (let [ns-name (ns-name namesp)
        _ (println "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        _ (println "path: " path)
        ;; fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (println "fn: " fn)
        ]
    ;; (str path "/" fn)))
    path))

(defn var->path [v]
  ;; (println "var->path: " v)
  (let [nm (:name (meta v))
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ;; _ (println "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (println "path: " path)
        fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (println "fn: " fn)
        ]
    (str path "/" fn)))

(defn sym->cljs-ns
  [sym]
  ;; (println "sym->cljs-ns: " sym)
  (let [v (resolve sym)
        nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defn var->cljs-ns
  [v]
  ;; (println "var->cljs-ns: " v (type v))
  (let [nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defn var->sym
  [v]
  ;; (println "var->sym: " v)
  (let [nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defmacro interface-sym->protocol-sym
  "Protocol names cannot contain '.'"
  [sym]
  ;; (println "interface-sym->protocol-sym: " sym)
  `(if (var? (resolve ~sym))
     (symbol (str (->> (resolve ~sym) meta :ns))
            (str (->> (resolve ~sym) meta :name)))
     (let [nodes# (str/split (str ~sym) #"\.")
           ns# (str/join "." (drop-last nodes#))
           ;; _# (println "ns: " ns#)
           nm# (last nodes#)
           newsym# (symbol (str ns#) (str nm#))]
       ;; (println "interface-sym->protocol-sym: " ~sym " => " newsym#)
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

(defn normalize-methods
  [methods]
  ;; (println "NORMALIZE-METHODS")
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
  ;; (println "NORMALIZE-RAWMETHODS")
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
    ;; (println "FILTERED BEHAVIORS: " ls)
    (if (seq ls)
      {:behaviors
       (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
         (if (nil? proto)
           result
           (let [proto (interface-sym->protocol-sym proto)
                 ;; _ (println "BEH META: " (meta (resolve proto)))
                 resource-type (:resource-type (meta (resolve proto)))
                 resource-name (:resource-name (meta (resolve proto)))
                 methods (take-while seq? tail)
                 next-proto (drop-while seq? tail)]
             (recur (first next-proto)
                    (next next-proto)
                    (if (= :polymer-behaviors resource-type)
                      (merge result {resource-name (let [ms (normalize-methods methods)]
                                                     ;; (println "NORMED BEHS: " ms)
                                                     ms)})
                      result)))))})))

(defn protos->rawlisteners
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (println "FILTERED LISTENERS: " ls)
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
                                             ;; (println "NORMED LISTENERS: " ms)
                                             ms)})
                      result)))))})))

(defn protos->rawmethods
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (:co-protocol?
                                 (meta (resolve (interface-sym->protocol-sym p))))))
                   protos)]
    ;; (println "FILTERED METHODS: " ls)
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
                                                      ;; (println "NORMED METHS: " ms)
                                                      ms)})
                               result)))))}]
        ;; (println "METHODS X: " ms)
        ms))))

(defn protos->behaviorvec
  [protos]
  ;; (println "PROTOS->BEHAVIORVEC: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-behaviors
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (println "FILTERED BEHAVIORS: " ls)
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
                           ;; (println "LISTENER PROTO: " proto)
                           ;; (println "LISTENER TYPE: " resource-type)
                           ;; (println "LISTENER TAIL: " tail)
                           ;; (println "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (println "LISTENER METHODS: " methods (type methods))
                             (println "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (println "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (println "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (println "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (println "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-behaviors resource-type)
                                        (concat result meths)
                                        result))))))))
           })))

(defn construct-behaviors-js
  [protocols]
  ;; (println "CONSTRUCT-BEHAVIORS-JS: " protocols)
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
                         (println "P META: " proto
                                  (meta (resolve (interface-sym->protocol-sym proto))))

                         (str (:resource-name (meta (resolve
                                                     (interface-sym->protocol-sym proto))))))))
           "\n\t  ]"))))

(defn construct-listeners-js
  [protos]
  (println "CONSTRUCT-LISTENERS-JS: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    (println "FILTERED LISTENERS: " ls)
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
                           (println "LISTENER PROTO: " proto)
                           (println "LISTENER TYPE: " resource-type)
                           (println "LISTENER TAIL: " tail)
                           (println "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (println "LISTENER METHODS: " methods (type methods))
                             (println "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (println "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (println "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (println "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (println "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-events resource-type)
                                        (concat result meths)
                                        result))))))))
           "\n\t  }"))))

(defn- props->propmap
  [args]
  ;; (println "PROPS->PROPMAP: " args)
  (let [props (filter (fn [x] (and (symbol? x)
                                       (:properties (meta (resolve x))))) args)]
    ;; (println "PROPS: " props)
    (if props
      (let [properties (into {} (for [prop props]
                                  (do ;; (println "PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (println "PVAR: " pvar)
                                            ps (:props (deref pvar))
                                            ;; _ (println "PS: " ps)
                                            ]
                                        ps))))
            html-attrs (into {} (for [prop props]
                                  (do ;; (println "HTMLATTRS PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (println "PVAR: " pvar)
                                            html-attrs (:html-attrs (deref pvar))
                                            ;; _ (println "HTMLATTRS: " html-attrs)
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
  (println "GET-PROTO-CODEFS: " opts+specs)
  (let [links (behaviors->elements opts+specs)]
    ;;[interfaces methods opts] (behaviors->elements opts+specs)]
    ;; (println "INTERFACES: " interfaces)
    ;; (println "METHODS: " methods)
    ;; (println "OPTS: " opts)
    links))

(defn- parse-cotype-args
  [args]
  ;; (println "PARSE COTYPE ARGS: " (pr-str args))
  (let [[docstr args] (if (string? (first args))
                        [(first args) (rest args)]
                        ["" args])
        codom (let [cd (filter (fn [x] (if (symbol? x)
                                         (:codom (meta (resolve x))))) args)]
                (if cd
                  cd ;; else get the (codom ...) form
                  ))
        _ (if (> (count codom) 1) (throw (IllegalArgumentException. (str "Only one codom arg allowed"))))
        props (let [props (filter (fn [x] (if (symbol? x)
                                            (:properties (meta (resolve x))))) args)]
                (if props
                  props
                  ))
        ;; _ (println "PROPERTIES: " props)
        ;; exclude docstring and codom
        protos (filter (fn [x] (if (or (list? x)
                                       (and (symbol? x)
                                            (not (:codom (meta (resolve x))))))
                                 x)) args)
        ;; behaviors
        ]
    [docstr args codom protos]))

;; from clojure/core_deftype.clj
(defn- parse-opts [s]
  ;; (println "parse-opts: " s)
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  ;; (println "PARSE-IMPLS: " specs)
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
  (println "->protomap: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        _ (println "OPTS: " opts)
        _ (println "SPECS: " specs)
        impls (parse-impls (first specs))
        _ (println "IMPLS: " impls (count impls))
        sigs (into {} (map (fn [arg]
                             (let [_ (println "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        _ (println "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               (println "PROTO: " psym)
                               (println "PROTO var: " (resolve psym))
                               (println "meta PROTO var: " (meta (resolve psym)))
                               (println "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        _ (println "URIs: " uris)
        interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        _ (println "INTERFACES: " interfaces)
        _ (doseq [intf interfaces] (println "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (println "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    (doseq [[k v] impls]
      (let [proto (interface-sym->protocol-sym k)
            sig (get sigs proto)]
        (println "PARSING PROTOCOL: " sig)
        (doseq [method-impl v]
          (println "interface sym: " k)
          (println "method-impl: " method-impl)
          (let [method-kw (if (= 'with-element (first method-impl))
                            (keyword (first (first (nnext method-impl))))
                            (keyword (first method-impl)))
                _ (println "method-kw: " method-kw)
                method-sig (get sig method-kw)
                method-impl (if (= 'with-element (first method-impl))
                              (next (first (nnext method-impl)))
                              method-impl)]
            (println "method-impl: " method-impl)
            (println "method-sig: " method-sig)
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
                (println "PROT-ARITIES: " proto-arities)
                #_(if (not-any? proto-arities [impl-arity])
                    (throw (Exception. (str "Bad arity: " method-impl " v. " method-sig))))))))))
    (for [[proto uri] uris]
      (if uri
        (x/element :link {:rel "import" :href uri})
        (x/element :link {:rel "import" :href (str proto)})))))

(defn- behaviors->elements [opts+specs]
  "emit elements for protos"
  ;; (println "behaviors->elements: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        ;; _ (println "OPTS: " opts)
        ;; _ (println "SPECS: " specs)
        impls (parse-impls (first specs))
        ;; _ (println "IMPLS: " impls (count impls))
        ;; obtain sig defns by resolving the protocol symbol
        sigs (into {} (map (fn [arg]
                             (let [;;_ (println "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        ;; _ (println "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               ;; (println "PROTO: " psym)
                               ;; (println "PROTO var: " (resolve psym))
                               ;; (println "meta PROTO var: " (meta (resolve psym)))
                               ;; (println "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        ;; _ (println "URIs: " uris)
        ;; interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
        ;;                set
        ;;                (disj 'Object 'java.lang.Object)
        ;;                vec)
        ;; _ (println "INTERFACES: " interfaces)
        ;; _ (doseq [intf interfaces] (println "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (println "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    ;; (doseq [[k v] impls]
    ;;   (let [proto (interface-sym->protocol-sym k)
    ;;         sig (get sigs proto)]
    ;;     (println "PARSING PROTOCOL: " sig)
    ;;     (doseq [method-impl v]
    ;;       (println "interface sym: " k)
    ;;       (println "method-impl: " method-impl)
    ;;       (let [method-kw (if (= 'with-element (first method-impl))
    ;;                         (keyword (first (first (nnext method-impl))))
    ;;                         (keyword (first method-impl)))
    ;;             _ (println "method-kw: " method-kw)
    ;;             method-sig (get sig method-kw)
    ;;             method-impl (if (= 'with-element (first method-impl))
    ;;                           (next (first (nnext method-impl)))
    ;;                           method-impl)]
    ;;         (println "method-impl: " method-impl)
    ;;         (println "method-sig: " method-sig)
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
    ;;             (println "PROT-ARITIES: " proto-arities)
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
           (println "DATE ARG: " x (type x))
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
          (println "Warning: protocol" protocol-var "is overwriting"
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
        ;; _ (println "PRESIGS: " sigs)
        html-attrs (first (filter map? sigs))
        ;; _ (println "HTMLATTRS: " html-attrs)
        sigs (filter list? sigs)
        ;; _ (println "LIST PRESIGS: " sigs)
        all-props (->> (map first (filter #(:tag (meta (first %))) sigs)))
        ;; _ (println "all-props: " all-props)
        sigs (when sigs
               (reduce1 (fn [m s]
                          ;; (println "REDUCE1 M: " m)
                          ;; (println "REDUCE1 S: " s)
                          (let [name-meta (meta (first s))
                                ;; _ (println "name-meta: " name-meta)
                                name-meta (dissoc (assoc name-meta :type (:tag name-meta))
                                                              :tag)
                                ;; _ (println "name-meta: " name-meta)
                                doc (:doc name-meta)
                                prop-type (if (some (set [(:type name-meta)])
                                                    (set (keys property-types)))
                                              (:type name-meta)
                                              'Observer)
                                ;; _ (println "prop-type: " prop-type)

                                name-meta (if (= prop-type 'Observer)
                                            (assoc name-meta :type 'Observer)
                                            name-meta)
                                ;; _ (println "name-meta: " name-meta)
                                mname (with-meta (first s) nil)
                                ;; _ (println "mname: " mname)

                                _ (let [strs (filter string? s)]
                                    ;; (println "STRINGS: " strs)
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
                                                        (do ;;(println "COMPUTED " (first (nnext s)))
                                                            (first (nnext s)))
                                                        (throw (IllegalArgumentException.
                                                                (str "Computed value for "
                                                                     mname
                                                                     " must be a fn."))))
                                                      (if (= prop-type 'Observer)
                                                        nil
                                                        (first (rest s))))]
                                              #_(println "V: " v (fn? (eval v)))
                                              (if (list? v) ;; a fn?
                                                (if (= (first v) 'fn)
                                                  (first (nnext v))
                                                  v)
                                                #_(throw (Exception. (str "don't understand " v))))
                                              (if (= prop-type 'Date)
                                                v
                                                #_(.toString (apply t/date-time v))
                                                v))
                                ;; _ (println "default-val: " default-val (type default-val))
                                _ (if (not= prop-type 'Observer)
                                    (if (not (nil? default-val))
                                      (let [pred (get property-types prop-type)]
                                        ;; (println "pred: " pred)
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
                                ;; _ (println "flags: " flags)
                                _  (if (some #{:computed} flags)
                                     (if (not (= :computed (nth s 1)))
                                       (throw (Exception. (str "Flag :computed must be first arg: "
                                                               mname)))))
                                observer (let [obs (if (some #{:computed} flags)
                                                     (do ;;(println "COMPUTED HIT")
                                                         (filter list? (next (nnext s))))
                                                     (if (= prop-type 'Observer)
                                                       (list (conj (rest s) 'fn))
                                                       (filter list? s)))]
                                           obs)
                                ;; doc (let [ss (filter string? s)]
                                ;;       (if (= 2 (count ss))
                                ;;         (last ss)
                                ;;         (if (= (first (next s)) (first ss)) nil (first ss))))
                                ;; _ (println "observer: " observer) ; (-> (first observer) next first))
                                ;; _ (println "doc: " doc)
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
                                  ;; (println "OBSERVER ARGS: " args)
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

        ;; _ (println "SIGS: " sigs)
        ;; meths (mapcat (fn [sig]
        ;;                 (let [m (munge (:name sig))]
        ;;                   (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object)
        ;;                        (:observer sig))))
        ;;               (vals sigs))
        ;; _ (println "METHS: " meths)
        ]
    ;; (println "DEFPROPS A")
  `(do
     (defonce ~name {})
     ;; (gen-interface :name ~iname :methods ~meths)
     (alter-meta! (var ~name) assoc :doc ~(:doc opts) :properties true)
     ~(when sigs
        `(#'assert-same-protocol (var ~name) '~(map :name (vals sigs))))
     ;; (println "DEFPROPS VAR: " (var ~name))
     ;; (println "DEFPROPS OPTS: " '~opts)
     ;; (println "DEFPROPS SIGS: " (str '~sigs))
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
    ;; (println "DEFPROPS X")
    ;;  (-reset-methods ~name)
    ;; (println "DEFPROPS " '~name)
     '~name)))

(defmacro defweb-properties
  [name & opts+sigs]
  ;; (println "DEFPROPERTIES: " name opts+sigs)
  (try (emit-properties name opts+sigs)
       (catch Exception e
         (throw (IllegalArgumentException. (str "defproperties " name ": " (.getMessage e)))))))

;; (defn require-foreign
;;   "1. clojure.core/require the ns  2. generate the <link> elts
;;   NB: for clojure.core/require to work the :refer items must be def'd in the ns
;;   so to support jit loading we need to require first w/o :refer, then define the :refers,
;;   then alias as needed"
;;   [& args]
;;   (println "REQUIREing " args)
;;   ;; step 1: clojure.core/require the namespaces, without options
;;   (doseq [arg args]
;;     (let [ns-basic (first arg)
;;           segs (str/split (str ns-basic) #"\.")]
;;          (println "CLOJURE.CORE/REQUIRE: " ns-basic)
;;          (try (clojure.core/require ns-basic :reload)
;;               (catch java.io.FileNotFoundException e
;;                 (throw (Exception.
;;                         (str "miraj.markup/require ns undefined: " (.getMessage e))))))
;;          (doseq [[isym ivar] (ns-interns ns-basic)] (println "INTERNED: " isym ivar))
;;          (doseq [[isym ivar] (ns-aliases ns-basic)] (println "ALIAS: " isym ivar))
;;          ;; make sure file actually has ns decl
;;          (if (find-ns ns-basic) nil (throw (Exception. (str "ns not declared: " ns-basic))))

;;          ;; make sure components map is defined
;;          ;; (if (not (ns-resolve ns-basic (symbol "components")))
;;          ;;   (throw (Exception. (str "components map not defined in : " ns-basic))))
;;          ;; step 2: resolve the referenced syms and generate html element fns
;;          (x/resolve-require-refs arg)))

;;   ;; step 3: for each :refer, generate a <link> element
;;   ;; require-resource does both
;;   ;; (println "EXPANDING REQUIRE")
;;   (do
;;      ;; (println "REQUIRing: " [~@args])
;;      (let [link-elts (for [arg args]
;;                         (do ;;(println "GET-REQ: " arg)
;;                           (let [r (x/require-resource arg)]
;;                             (doall r)
;;                             r)))]
;;        (doall link-elts)
;;        ;; (println "REQUIREd: " link-elts#)
;;                           link-elts)))

(defn require-polymer
  [req] ;; e.g.  [polymer.paper :as paper :refer [button card]]
  (println "require-polymer: " req)
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
  ;; (println "refer: " ns-sym filters)
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
            ;; (println "REFER " sym v (meta v))
            (. *ns* (refer (or (rename sym) sym) v))
            (:uri (meta v))
            #_(str "bower_components/" (:uri (meta v)))))))))

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

(defn construct-html
  [ns-sym nm-sym type & docstring]
  ;; (println "CONSTRUCT-HTML:" ns-sym nm-sym docstring) ;; elt-kw uri docstring)
  (let [ds (if (empty? docstring) ""
               (if (string? (first docstring))
                 (first docstring)
                 (if (vector? (first docstring))
                   (if (empty? (last (first docstring))) "" (last (first docstring))))))
        segs (str/split (str ns-sym) #"\.")]
    (if (= (first segs) "polymer")
      (let [polymer-cat (last segs)
            ;; _ (println "polymer category: " polymer-cat)
            html-tag (str polymer-cat "-" nm-sym)
            ;; _ (println "html-tag: " html-tag)
            html-kw (keyword html-tag)
            uri (str "bower_components/" (if (vector? (first docstring))
                                           (ffirst docstring)
                                           (str html-tag "/" html-tag ".html")))
            ;; _ (println "uri:      " uri)
            newvar (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds :uri uri type true})
                           (fn [& args]
                             (let [elt (if (empty? args)
                                         (do ;; (println "COMPONENT FN NO ARGS: " html-kw)
                                             (x/element html-kw))
                                         (let [first (first args)
                                               rest (rest args)
                                               [attrs content] (x/parse-elt-args first rest)]
                                           (apply x/element html-kw attrs content)))]
                               elt)))]
        ;; (println "NS-SYM: " ns-sym)
        ;; (println "NM-SYM: " nm-sym)
        ;; (println "VAR: " newvar)
        newvar)
      ;; else custom component
      (let [html-tag nm-sym
            html-kw (keyword html-tag)
            uri (str (str/replace (str ns-sym) #"\." "/") "/" nm-sym ".html")
            newvar (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds :uri uri type true})
                           (fn [& args]
                             (let [elt (if (empty? args)
                                         (do ;; (println "COMPONENT FN NO ARGS: " html-kw)
                                             (x/element html-kw))
                                         (let [first (first args)
                                               rest (rest args)
                                               [attrs content] (x/parse-elt-args first rest)]
                                           (apply x/element html-kw attrs content)))]
                               elt)))]
        ;; (println "NS-SYM: " ns-sym)
        ;; (println "NM-SYM: " nm-sym)
        ;; (println "VAR: " newvar)
        newvar))))

(defn- load-polymer-lib
  [lib need-ns require]
  ;; (println "load-polymer-lib: " lib need-ns require)
  (let [lns (create-ns lib)
        segs (str/split (str lib) #"\.")
        pns (find-ns 'miraj.polymer)
        v (find-var (symbol "miraj.polymer" (str need-ns)))
        kw-docstring-map (deref v)]
    ;; (println "created ns:  " lns)
    ;; (println "polymer ns:  " pns)
    ;; (println "polymer var: " v)
    ;; (println "var val: " kw-docstring-map)
    (if need-ns
      (do ;; (println "making alias " need-ns lns)
        ;; (println "current ns: " *ns*)
        (alias (symbol need-ns) (symbol lib))
        #_(println "ALIASES: " (ns-aliases *ns*))))
    (doseq [[kw docstring] kw-docstring-map]
      (do
        (let [sym (symbol (name kw))
              elt (construct-html lns sym :polymer docstring)]
          ;; (println "ELT: " elt)
          #_(load (root-resource lib))
          (throw-if (and need-ns (not (find-ns lib)))
                    "namespace '%s' not found after loading '%s'"
                    lib lib #_(root-resource lib))
          (when require
            (dosync
             (commute *loaded-libs* conj lib)))
          elt)))))

(defn- load-custom-lib
  [lib need-ns require opts]
  ;; (println "load-custom-lib: " lib need-ns require opts)
  (let [lns (create-ns lib)]
        ;; segs (str/split (str lib) #"\.")
        ;; pns (symbol "miraj.polymer" (str (last segs)))]
    ;; (println "created ns: " lns)
    (if need-ns
      (alias (symbol need-ns) (symbol lib)))
    ;; TODO: find and load jarfile?
    (doseq [sym (:refer (apply hash-map opts))]
      (let [elt (construct-html lns sym :cowebcomponent "")]
        #_(load (root-resource lib))
        (throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found after loading '%s'"
                  lib lib #_(root-resource lib))
        (when require
          (dosync
           (commute *loaded-libs* conj lib)))
        elt))))

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib need-ns require opts]
  ;; (println "load-one " lib need-ns require)
  (let [segs (str/split (str lib) #"\.")
        seg1 (first segs)]
    ;; (println "segs: " segs)
    ;; (println "seg1: " seg1)
    (if (= seg1 "polymer")
      (let [pl (load-polymer-lib lib need-ns require)]
        ;; (println "PL: " pl)
        pl)
      (let [cl (load-custom-lib lib need-ns require opts)]
        ;; (println "custom lib: " cl)
        cl)
      )))

;; (let [lns (create-ns lib)]
;;     (println "created ns: " lns)
;;     (if need-ns
;;       (do ;; (println "making alias " need-ns lns)
;;           ;; (println "current ns: " *ns*)
;;           (alias (symbol need-ns) (symbol lib))
;;           #_(println "ALIASES: " (ns-aliases *ns*)))))
;;   ;; (println "polymer/paper: " polymer/paper)
;;   (println "POLYMER/PAPER: " (find-var (symbol (str/replace (str lib) #"\." "/"))))
;;   (let [tbl (symbol (str/replace (str lib) #"\." "/"))]
;;     (println "tbl: " tbl)
;;     (let [table tbl] ;; (find-var tbl)]
;;       (doseq [sym (eval table)]
;;         (println (str "SYM: " sym)))))
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
  ;; (println "load-all " lib need-ns require)
  (dosync
   (commute *loaded-libs* #(reduce1 conj %1 %2)
            (binding [*loaded-libs* (ref (sorted-set))]
              (load-one lib need-ns require)
              @*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  ;; (println "load-lib: " prefix lib options)
  (throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @*loaded-libs* lib)
        ;; _ (println "lib already loaded? " lib loaded)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    ;; (println "LOAD: " load)
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (try
          (load lib need-ns require (mapcat seq filter-opts))
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        #_(throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      (when (and need-ns *loading-verbosely*)
        (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when *loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as lib))
        #_(alias as lib))
      (when (or use (:refer filter-opts))
        (when *loading-verbosely*
          (printf "(miraj.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (let [result (apply refer lib (mapcat seq filter-opts))]
          ;; (println "REFER RESULT: " result)
          result)))))

(defn- load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  ;; (println "LOAD-LIBS " args)
  ;; step 1: clojure.core/require the namespaces, without options
  ;;(doseq [arg args]
    (let [;; ns-basic (first arg)
          ;; segs (str/split (str ns-basic) #"\.")
          flags (filter keyword? args)
          opts (interleave flags (repeat true))
          args (filter (complement keyword?) args)]
      ;; (println "MIRAJ.CORE/REQUIRE: " #_ns-basic)
      ;; (println "flags: " flags)
      ;; (println "opts:  " opts)
      ;; (println "args:  " args)
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

(defn require
  ""
  [page-var & args]
  ;; (println ":REQUIRE " page-var args)
  (let [reqres (remove nil? (flatten (apply load-libs :require args)))
        reqelts (for [req reqres]
                  (x/element :link {:rel "import" :href req}))]
    ;; (println "REQUIRE RESULT: " reqelts)
    (alter-meta! page-var (fn [old] (assoc old :_webcomponents args)))
    [:require reqelts]))

;;obsolete
#_(defn import-resources
  [resources imports-map]
  ;; (println "import-resource: " resources imports-map)
  (let [ns (first resources)
        ;; _ (println "ns: " ns)
        resources (next resources)
        ;; _ (println "resources: " resources)
        ]
    ))

(defn get-imports-config
  []
  (let [home-imports-path (str (System/getProperty "user.home") "/.miraj/imports.edn")
        ;; _ (println "home-imports-path: " home-imports-path)
        home-imports (try (slurp home-imports-path)
                          (catch java.io.FileNotFoundException e
                            (do (println "EXCEPTION: " (.getMessage e)) nil)))
        home-imports (if home-imports (read-string home-imports) nil)
        ;; _ (println "HOME IMPORTS.EDN: " home-imports)
        proj-imports (try (slurp ".miraj/imports.edn")
                          (catch java.io.FileNotFoundException e
                            (do (println "EXCEPTION: " (.getMessage e)) nil)))
        proj-imports (if proj-imports (read-string proj-imports) nil)
        ;; _ (println "PROJ IMPORTS.EDN: " proj-imports)
        ]
    (merge-with merge home-imports proj-imports)))

(defn import
  ""
  [page-var & args]
  ;; (println ":IMPORT " page-var args)
  (let [imports-config-map (get-imports-config)
        ;; _ (println "IMPORTS CONFIG MAP: " imports-config-map)
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
    ;; (println "IMPORTS: " imports)
    (alter-meta! page-var (fn [old] (assoc old :_webimports args)))
    [:import imports]))

      ;; (if (= (first segs) "polymer")
      ;;   (require-polymer arg)))))

  ;;     (try (clojure.core/require ns-basic :reload)
  ;;             (catch java.io.FileNotFoundException e
  ;;               (throw (Exception.
  ;;                       (str "miraj.markup/require ns undefined: " (.getMessage e))))))
  ;;        (doseq [[isym ivar] (ns-interns ns-basic)] (println "INTERNED: " isym ivar))
  ;;        (doseq [[isym ivar] (ns-aliases ns-basic)] (println "ALIAS: " isym ivar))
  ;;        ;; make sure file actually has ns decl
  ;;        (if (find-ns ns-basic) nil (throw (Exception. (str "ns not declared: " ns-basic))))

  ;;        ;; make sure components map is defined
  ;;        ;; (if (not (ns-resolve ns-basic (symbol "components")))
  ;;        ;;   (throw (Exception. (str "components map not defined in : " ns-basic))))
  ;;        ;; step 2: resolve the referenced syms and generate html element fns
  ;;        (x/resolve-require-refs arg)))

  ;; ;; step 3: for each :refer, generate a <link> element
  ;; ;; require-resource does both
  ;; ;; (println "EXPANDING REQUIRE")
  ;; (do
  ;;    ;; (println "REQUIRing: " [~@args])
  ;;    (let [link-elts (for [arg args]
  ;;                       (do ;;(println "GET-REQ: " arg)
  ;;                         (let [r (x/require-resource arg)]
  ;;                           (doall r)
  ;;                           r)))]
  ;;      (doall link-elts)
  ;;      ;; (println "REQUIREd: " link-elts#)
  ;;                         link-elts)))

(defn body
  ""
  [page-var & args]
  (let [content (map #(eval %) args)]
        ;;bod (x/element :body content)]
    ;; (println ":BODY " content)
    [:body content]))

(defmacro defweb-page
  ""
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  (println "defweb-page " name " in ns " *ns*)
  (let [page-var (intern *ns* name)
        ;; _ (println "PAGE VAR: " page-var)
        process-reference
        (fn [[kname & args]]
          `(do
             ;; (println "PROCESSING: " '~(symbol "miraj.core" (clojure.core/name kname)))
             ;; (println "ARGS: " '~args)
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
        ;; _ (println "metadata: " metadata)
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        ;; _ (println "defweb-page NAME: " name)
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
         (let [reqs# (into {} [~@(map process-reference references)])
               head# (apply x/element :head {} (vec (flatten (list (:require reqs#) (:import reqs#)))))
               ;; _# (println "HEAD# " head#)
               body# (apply x/element :body {} (:body reqs#))
               html# (apply x/element :html {} (vec (flatten (list head# body#))))
               ]
           ;; (println "ALL: " reqs#)

           ;; (println "BODY# " body#)
           ;; (println "HTML# " html#)
           ;; (clojure.core/intern *ns* '~name html#)
           ;; (intern *ns*
           ;;         (with-meta
           ;;           (symbol ~(str name))
           ;;           (merge {:doc ~docstring :_webpage true} ~metadata)))
           (clojure.core/alter-var-root ~page-var
                                        (fn [old# & args#] html#))
           (clojure.core/alter-meta! ~page-var
                                     (fn [old# & args#] (merge old# ~metadata {:doc ~docstring :_webpage true}))))))))

;;OBSOLETE
#_(defmacro co-fn
  [fn-tag docstring elt-kw elt-uri typ]
  (do #_(println "co-fn:" typ fn-tag elt-kw elt-uri docstring)
              (eval `(defn ~fn-tag ~docstring
                       [& args#]
;;                       (println "invoking " ~fn-tag)
                       (let [elt# (if (empty? args#)
                                    (with-meta (element ~elt-kw)
                                      {:miraj
                                       {:co-fn true
                                        :component ~typ
                                        :doc ~docstring
                                        :elt-kw ~elt-kw
                                        :elt-uri ~elt-uri}})
                                    (let [first# (first args#)
                                          rest# (rest args#)
                                          [attrs# content#] (parse-elt-args first# rest#)]
                                      (with-meta (apply element ~elt-kw attrs# content#)
                                        {:miraj {:co-fn true
                                                 :component ~typ
                                                 :doc ~docstring
                                                 :elt-kw ~elt-kw
                                                 :elt-uri ~elt-uri}})))]
                         elt#)))
              (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
                            (fn [old new]
                              (merge old new))
                            {:miraj {:co-fn true
                                     :component typ
                                     :doc docstring
                                     :elt-kw elt-kw
                                     :elt-uri elt-uri}})))

(defmacro defweb-codom
  [nm & args]
  ;; (println "DEF-CODOM: " (str nm)) ;; " ARGS: " args)
  (let [ns-sym *ns* ; (ns-name *ns*)
        nm-sym (symbol (name nm))
        [docstr args] (if (string? (first args))
                        [(first args) (rest args)]
                        ["" args])
        [argvec args] [(first args) (rest args)]]
    ;; (println "    NS: " (pr-str ns-sym))
    ;; (println "    NM_SYM: " (pr-str nm-sym))
    ;; (println "    DOCSTR: " (pr-str docstr))
    ;; (println "    DOCSTR: " (pr-str docstr))
    ;; (println "    ARGVEC: " (pr-str argvec))
    ;; (println "    ARGS: " (pr-str args))
  `(do ;;(println "codom: " ~(str nm) ~@args)
       (let [tree# (apply x/element ;;~(keyword nm)
                          :CODOM_56477342333109
                          {:id ~(str nm)} (list ~@args))
             codom# (x/xsl-xform x/xsl-normalize-codom tree#)]
         (intern *ns* (with-meta (symbol ~(str nm-sym)) {:doc ~docstr :codom true})
                 codom#)
         codom#))))

(defn props->cljs
  [propmap]
  ;; (println "PROPS->CLJS: " propmap)
  ;; (if (not (:props propmap))
  ;;   (throw (IllegalArgumentException. (str "props->cljs arg must be a Properties map"))))
  (let [props (:properties propmap)
        prop-keys (keys props)
        html-attrs (:html-attrs propmap)]
    ;; (println (str (:on propmap) ": " prop-keys))
    {:hostAttributes html-attrs
     :properties (into {}
                       (for [prop-key prop-keys]
                         (let [prop (get props prop-key)
                               ;; _ (println "PROP: " prop)
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
                               ;; _ (println "FLAGS: " flags)
                               ]
                           ;; (println "procesing property: " (pr-str prop))
                           ;; (println "descriptors: " descriptors)
                           {(keyword (:name prop)) (merge type+val flags
                                                          (if (:observer prop)
                                                            {:observer (first (:observer prop))}))})))}))

(defn listeners->cljs
  [ls]
  ;; (println "LISTENERS->CLJS: " (:listeners ls))
  {:listeners
   (into {}
         (merge
          (concat
           (flatten (for [[evsuite listeners] (:listeners ls)]
                      (do ;;(println "EVENT SUITE: " evsuite)
                        (for [[ev listener] listeners]
                          (do ;;(println "EV/LISTENER: " ev "/" listener)
                            {ev (keyword (first listener))}
                            ))))))))
         })

(defn methods->cljs
  [ms]
  ;; (println "METHODS->CLJS: " ms)
  (let [observers (map #(:observer %)
                       (filter #(:observer %) (vals (:properties ms))))
        ;; _ (println "OBSERVERS: " observers)
        ls (vals (reduce merge (vals (:listeners ms))))
        ;; _ (println "LISTENER METHODS: " ls)
        behs (vals (reduce merge (vals (:behaviors ms))))
        ;; _ (println "BEH METHODS: " behs)
        this-methods (vals (reduce merge (vals (:methods ms))))
        ;; _ (println "THIS METHODS: " this-methods)
        methods (concat observers ls behs this-methods)
        ;; _ (println "METHODS: " methods)
        ]
    (into {} methods)))

  ;; {:methods
  ;;  (into {}
  ;;        (merge
  ;;         (concat
  ;;          (flatten (for [[evsuite listeners] (:listeners ms)]
  ;;                     (do ;;(println "EVENT SUITE: " evsuite)
  ;;                       (for [[ev listener] listeners]
  ;;                         (do ;;(println "EV/LISTENER: " ev "/" listener)
  ;;                           {ev (keyword (first listener))}
  ;;                           ))))))))
  ;;        })

(defn behaviors->cljs
  [behs]
  ;; (println "BEHAVIORS->CLJS: " (:behaviors behs))
  (let [behaviors (keys  (:behaviors behs))]
    {:behaviors (into [] (map keyword behaviors))}))

(defn component->prototype
  [cvar rawprops rawlisteners rawbehaviors rawmethods]
  ;; (println "COMPONENT->PROTOTYPE: " cvar (type cvar))
  ;; (println "RAWLISTENERS: " rawlisteners)
  ;; (println "RAWMETHODS: " rawmethods)
  ;; (println "RAWBEHAVIORS: " rawbehaviors)
  (let [;; v (resolve nm)
        ;; _ (println "component var: " cvar)
        ;; _ (println "props: " props)
        ;; namesp (:ns (meta cvar))
        ;; ns-name (ns-name namesp)
        uri (str "tmp/" (var->path cvar) ".cljs")
        cljs-ns (var->cljs-ns cvar)
        propmap (props->cljs rawprops)
        listeners (listeners->cljs rawlisteners)
        methmap (methods->cljs (merge rawprops rawlisteners rawbehaviors rawmethods))
        ;; _ (println "METHODS MAP: " methmap)
        behaviors (behaviors->cljs rawbehaviors)
        cljs (str/join "\n" [(pprint-str (list 'ns cljs-ns))
                             ;; (pprint-str '(enable-console-print!))
                             (pprint-str (list 'js/Polymer
                                               (list 'clj->js
                                                     (merge {:is (keyword (:name (meta cvar)))}
                                                            propmap
                                                            listeners
                                                            methmap
                                                            behaviors))))])]
    ;; (println "CLJS:\n" cljs)
    ;; (println "URI: " uri)
    ;; (io/make-parents uri)
    ;; (spit uri cljs)
    (alter-meta! cvar
               (fn [old new]
                 (merge old new))
               {:miraj {:prototype cljs}})))

(defmacro defweb-component
  "Define a web component (Polymer flavor)"
  [nm & args]
  ;; (println "COMPONENT: " (str nm)) ;; " ARGS: " args)
  (if (not (str/includes? nm "-")) (throw (IllegalArgumentException.
                                           (str "Component name must contain at least one dash '-'."))))
  ;; (println "COMPONENT ARGS: " (pr-str args))
  (let [[docstr arglist codom & protos] (parse-cotype-args args)
        ;; _ (println "DOCSTR: " (pr-str docstr))
        ;; _ (println "ARGLIST: " (pr-str arglist))
        ;; _ (println "COTYPE PROTOS: " protos (seq protos))
        ;; _ (println "CODOM: " codom)
        rawprops (props->propmap args)

        rawmethods (apply protos->rawmethods protos)

        rawbehaviors (apply protos->rawbehaviors protos)

        rawlisteners (apply protos->rawlisteners protos)

        ;; codom (drop 1 cod)
        behavior-elts (behaviors->elements protos)
        ;; _ (println (str "BEHAVIOR-ELTS: " (pr-str behavior-elts)))
        ;; protomap (->protomap protos)
        ;; _ (println "PROTOMAP: " protomap)

        ;; js-ctor (js-constructor nm arglist protos)
;;        js-ctor (element :script (str *ns* "." nm "()"))
        ]
    `(let [tree# ~@codom
       ;; html-constructor interns the name, binding it to ->html fn
           cvar# (x/html-constructor ~*ns* '~nm (keyword '~nm) (str (ns->uri ~*ns*) "/" '~nm))
           _# (apply component->prototype cvar# '~rawprops '~rawlisteners '~rawbehaviors '~rawmethods)
           ;; _# (println "PROTOTYPE: " (:prototype (:miraj (meta cvar#))))
             ;; (if (instance? miraj.markup.Element ~(first codom))
             ;;         ~@codom
             ;;         (codom ~nm ~@codom))
             content# (:content tree#)
             result# (update tree#
                            :content (fn [c#]
                                       (let [dom# (last c#)
                                             ;; _# (println "DOM#: " dom#)
                                             newdom# (update dom#
                                                             :attrs
                                                             (fn [old-attrs#]
                                                               (assoc old-attrs# :id (str '~nm))))
                                             ;; newdom# (update newdom#
                                             ;;                 :content (fn [domc#]
                                             ;;                            (concat domc# [~js-ctor])))
                                             ]
                                         ;; (println "NEWDOM#: " newdom#)
                                         (concat (butlast c#)
                                                 [~@behavior-elts]
                                                 #_[dom#]
                                                 [newdom#]
                                                 ))))
             ;; var# (find-var (symbol (str ~*ns*) (str '~nm)))
           ]
         ;; (println "ALTERING META FOR " cvar#)
         (alter-meta! cvar#
                      (fn [old# new#]
                        (do ;;(println "old#: " old#)
                           ;;(println "new#: " new#)
                            (merge old#
                                   {:miraj (merge (:miraj old#)
                                                  new#)})))
                      {:_webcomponent true
                       :codom result#})
         ;; (println "ALTERED META FOR " cvar#)
         cvar#)))

(defn- get-webcomponents
  "Search namespaces for webcomponents"
  [& nss]
  (println "get-webcomponenents for nss: " nss)
  (let [nss (or nss (all-ns))]
    (for [the-ns nss]
      ;; (println "NS: " the-ns)
      (let [interns-map (ns-interns the-ns)]
        (filter (fn [entry] (:_webcomponent (meta (last entry)))) interns-map)))))


;; #_(defn- get-components
;;   []
;;   (println "TASK: boot-miraj/extract")
;;   (let [root-dir (or root-output-dir "miraj")
;;         tmp-dir (io/file (core/tmp-dir!) root-dir)
;;         tmp-output-path  (.getPath tmp-dir)
;;         ;; _ (println "tmp-dir: " tmp-dir)
;;         html-output-dir (if html-output-dir (io/file tmp-dir html-output-dir) tmp-dir)
;;         html-output-path  (.getPath html-output-dir)
;;         cljs-output-dir   (if cljs-output-dir (io/file tmp-dir cljs-output-dir) tmp-dir)
;;         cljs-output-path  (.getPath cljs-output-dir)
;;         last-fileset (atom nil)
;;         ;; pod          (-> (core/get-env)
;;         ;;                  (update-in [:dependencies] into '[[miraj/markup "0.1.0-SNAPSHOT"]])
;;         ;;                  pod/make-pod  ;; use pod-pool?
;;         ;;                  future)
;;         ]
;;     (core/with-pre-wrap fileset
;;       (pod/with-eval-in @pod
;;         (require '[miraj.markup :as miraj])
;;         (do
;;           (if ~ns-str
;;             (let [ns-sym (symbol ~ns-str)]
;;               (require ns-sym)
;;               (let [interns (ns-interns ns-sym)
;;                     bld (resolve 'miraj.markup/build-component)]
;;                 (doseq [[isym ivar] interns]
;;                   (if (:webcomponent (meta ivar))
;;                     (miraj.markup/build-component [~html-output-path ~cljs-output-path]
;;                                                   [isym ivar]))))))
;;           (if ~component
;;             (do
;;               (boot.util/info "processing var: " ~component "\n")
;;               (let [widget (symbol ~component)
;;                     ns-sym (symbol (namespace widget))
;;                     nm (symbol (name widget))]
;;                 (require ns-sym)
;;                 (let [ivar (resolve widget)]
;;                   (if (:webcomponent (meta ivar))
;;                     (miraj/build-component [~html-output-path ~cljs-output-path]
;;                                            [nm ivar]))))))))
;;       (core/sync! root-dir tmp-dir)
;;       fileset)))

(defn- get-webvars
  "Search namespaces for vars"
  [web-var & nss]
  (println "get-webvars: " web-var nss)
  (let [nss (or nss (all-ns))]
    (for [the-ns nss]
      ;; (println "NS: " the-ns)
      (let [interns-map (ns-interns the-ns)]
        (filter (fn [entry] (get (meta (last entry)) web-var)) interns-map)))))

(def bower-repo "bower_components")

(defn- bowerize
  [bower-pkg assets-out]
  (println "bowerize " bower-pkg assets-out)
  ;;  (bow/install-bower :pkg-name entry :pkg-type :bower)
  (let [local-bower  (io/as-file "./node_modules/bower/bin/bower")
        global-bower (io/as-file "/usr/local/bin/bower")
        bcmd         (cond (.exists local-bower) (.getPath local-bower)
                           (.exists global-bower) (.getPath global-bower)
                           :else "bower")
        bower-dir (str assets-out bower-repo)]
    ;; (io/make-parents bower-dir)
    (let [c [bcmd "install" bower-pkg :dir assets-out]]
      (println "bower cmd: " c)
      (println (format "Installing bower pkg:   %s\n" bower-pkg))
      (apply sh c))))

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

(defn- compile-component-import
  "Compile web component import forms."
   [import imports-config-map assets-out]
  (println "compile-import: " import)
  (let [ns-basic (first import)
        items (apply hash-map (next import))
        segs (str/split (str ns-basic) #"\.")
        seg1 (first segs)
        seg2 (fnext segs)]
    (println "segs: " segs)
    (println "seg1: " seg1)
    (println "seg2: " seg2)
    (doseq [item (:refer items)]
      (println "ITEM: " item)
      (if (= seg1 "polymer")
        (let [pkg-name (str "PolymerElements/" seg2 "-" item)]
          (println "IMPORT POLYMER: " pkg-name)
          (bowerize pkg-name assets-out))
        (println "NONPOLYMER: " ns-basic item)))))

(defn- compile-imports
  "Process all webimports in the project."
  [assets-out & nss]
  (println "compile-imports")
  (let [pages (apply hash-map (flatten (remove empty? (apply get-webvars :_webpage nss))))
        imports-config-map (get-imports-config)]
    (println "PAGES: " pages)
    (doseq [[psym pvar] pages]
      (let [page-meta (meta pvar)]
        (doseq [import (:_webimports page-meta)]
          (compile-import import imports-config-map assets-out))
        (doseq [component (:_webcomponents page-meta)]
          (compile-component-import component imports-config-map assets-out))))))

(defn- compile-webcomponent
  "Compile defweb-component forms to html+js and write to files."
   [csym cvar html-out cljs-out]
  ;; [^clojure.lang.Symbol component & mode]
  ;; [file doc & mode]
  (println "compile-webcomponent: " csym cvar)
  (println "Output dirs: " html-out cljs-out)
  (let [path (str/replace (str (:ns (meta cvar))) #"\." "/")
        csym (str/replace csym #"-" "_")
        hfile (str/join "/" [html-out path (str csym ".html")])
        cfile (str/join "/" [cljs-out path (str csym ".cljs")])]
    (println "hfile: " hfile)
    (println "cfile: " cfile)
    (io/make-parents hfile)
    (io/make-parents cfile)
    (spit hfile (with-out-str (x/pprint (-> cvar meta :miraj :codom))))
    (spit cfile (-> cvar meta :miraj :prototype))))

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
    ;; (println "hfile: " hfile)
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
(defn webc
  "Compile clj+cljs to html+js. Optional args: html-out (string),
  cljs-out (string) nss (vector of ns symbols). With no args, process
  all namespaces and write output to './'.  Each namespace will be
  searched for defcomponent and defpage forms, which will be processed
  to generate HTML and Javascript files."
  [& {:keys [html-out cljs-out assets-out nss]
      :or   {html-out "./"
             cljs-out   "./"
             assets-out "./"
             nss nil}}]
  (println "webc " *ns*)
  ;(compile-webcomponents html-out cljs-out)
  ;(compile-webpages html-out cljs-out)
  (compile-imports assets-out))

(println "loaded miraj/core.clj")
