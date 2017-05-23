(ns ^{:doc "Miraj utility functions"
      :author "Gregg Reynolds"}
  miraj.utils
  (:refer-clojure :exclude [import refer require])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.data.json :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clj-time.core :as t]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            ;; [mobileink.boot-bowdlerize :as bow]
            ;; [polymer :refer :all]
            [miraj.co-dom :as codom]
            ;; [clojure.tools.reader :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer :as ana]
            ;; [cljs.compiler :as c]
            ;; [cljs.closure :as cc]
            ;; [cljs.env :as env])
            [clojure.tools.logging :as log :only [trace debug error info]]))

(defn namespace? [n]
  (instance? clojure.lang.Namespace *ns*))

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

(defn path->ns-sym [path]
  (let [ns-sym (str/replace path #"_|/" {"_" "-" "/" "."})
        ]
    (symbol ns-sym)))

(defn sym->path [sym]
  (let [path (str/replace sym #"-|\." {"-" "_" "." "/"})
        ]
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

(defn var->ns
  [v]
  (let [ns (-> v meta :ns ns-name)
        nm (-> v meta :name)]
    (symbol (str ns "." nm))))

(defn var->path [v]
  (let [ns-str (str (-> v meta :ns ns-name))
        ns-path (str/replace ns-str #"-|\." {"-" "_" "." "/"})
        name (-> v meta :name)
        name (str/replace name #"-|\." {"-" "_" "." "/"})
        ]
    (str ns-path "/" name)))

;; (defn var->path [v]
;;   ;; (log/debug "var->path: " v)
;;   (let [nm (:name (meta v))
;;         namesp (:ns (meta v))
;;         ;; _ (log/debug "namesp: " namesp)
;;         ns-name (ns-name namesp)
;;         ;; _ (log/debug "ns-name: " ns-name)
;;         path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
;;         ;; _ (log/debug "path: " path)
;;         fn (str/replace nm #"-|\." {"-" "_" "." "/"})
;;         ;; _ (log/debug "fn: " fn)
;;         ]
;;     (str path "/" fn)))

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

(defn var->varsym
  [v]
  ;; (log/debug "var->sym: " v)
  (let [nm (:name (meta v))
        ;; _ (log/debug "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (log/debug "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name) (str nm))))

(defn last-seg [sym]
  (if-let [nspace (namespace sym)]
    (name sym)
    (let [segs (str/split (str sym) #"\.")]
      (last segs))))
