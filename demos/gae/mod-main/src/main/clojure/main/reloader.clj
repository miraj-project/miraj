(ns main.reloader
  (:import (javax.servlet Filter FilterChain FilterConfig
                          ServletRequest ServletResponse))
  (:require [ns-tracker.core :refer :all]))


(defn -init [^Filter this ^FilterConfig cfg])

(defn -destroy [^Filter this])

;; For testing :ear:aRun: the paths to watch are relative to main/ear/build/exploded-app
;; make them match what's in modules/ear/src/main/application/META-INF/application.xml
;;  and modules/ear/src/main/application/META-INF/application.xml
(def main-namespaces (ns-tracker ["./mod-main-0.1.0/WEB-INF/classes/main"]))

;; For testing :mod-main:aRun: the paths to watch are relative to main/mod-main/build/exploded-app
;;(def main-namespaces (ns-tracker ["./build/classes/WEB-INF/classes/main/main"]))

(defn -doFilter
  [^Filter this
   ^ServletRequest rqst
   ^ServletResponse resp
   ^FilterChain chain]
  (println "running mod-main doFilter")
  (doseq [ns-sym (main-namespaces)]
    (do
      (println "ns changed: " ns-sym (type ns-sym))
      (let [sym (if (symbol? ns-sym)
                  ns-sym
                  (last ns-sym))]
      (require sym
               :reload
               ;;:verbose
               ))))
  (.doFilter chain rqst resp))

(clojure.core/println "loading mod-main main.reloader")
