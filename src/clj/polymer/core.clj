(ns polymer.core
  (:refer-clojure :exclude [list map meta time])
  (:require [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [cheshire.core :as json :refer :all]
            [clj-http.client :as client]))

;;(ns-unmap *ns* 'make-polymer-fns)

(defn make-polymer-fns
  [pfx args]
  (log/trace "make-polymer-fns " pfx) ;; " " args) ;; (type args))
  (doseq [arg args]
    (let [farg (symbol arg)
          kw (keyword (str pfx "-" arg))
          ;; log (println "make-polymer-fns arg: " farg " (" arg ")")
          func `(defn ~farg ;; (symbol (str arg))
                  [& hargs#]
                  ;; (println "POLYMER FN: " ~kw (pr-str hargs#))
                  (if (empty? hargs#)
                    (xml/element ~kw)
                    (let [first# (first hargs#)
                          attrs# (if (map? first#)
                                   (do ;(log/trace "map? first")
                                       (if (instance? clojure.data.xml.Element first#)
                                         (do ;(log/trace "Element instance")
                                             {})
                                         (do ;(log/trace "NOT Element instance")
                                             first#)))
                                   (do ;(log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? clojure.data.xml.Element first#)
                                       hargs#
                                       (rest hargs#))
                                     hargs#)
                          func# (apply xml/element ~kw attrs# content#)]
                      ;; (log/trace "hargs: " hargs#)
                      ;; (log/trace "kw: " ~kw)
                      ;; (log/trace "args: " attrs#)
                      ;; (log/trace "content: " content# " (" (type content#) ")")
                      ;; (log/trace "func: " func# (type func#))
                      func#)))
          f (eval func)])))

;; ContributionGuide
;; app-layout-templates
;; chartjs-element
;; font-roboto
;; font-roboto-local
;; marked-element
;; molecules
;; polymer-starter-kit
;; prism-element
;; seed-element
;; style-guide
;; test-all
;; test-benchmark
;; test-fixture


(def raw (atom []))
(def repos (atom {}))

;; (defn list-polymer-elts
;;   []
;;   (let [page (client/get "https://api.github.com/orgs/PolymerElements/repos")
;;         body (:body page)
;;         jsons (json/parse-string body true)]
;;     (swap! raw (fn [old-val new-val] (conj old-val new-val)) body)
;;     (doseq [json jsons]
;;       (swap! repos (fn [old-val new-val] (assoc old-val (:name new-val) (:full_name new-val))) json)))
;;   (let [page (client/get "https://api.github.com/organizations/11639138/repos?page=2")
;;         body (:body page)
;;         jsons (json/parse-string body true)]
;;     (swap! raw (fn [old-val new-val] (conj old-val new-val)) body)
;;     (doseq [json jsons]
;;       (swap! repos (fn [old-val new-val] (assoc old-val (:name new-val) (:full_name new-val))) json)))
;;   (let [page (client/get "https://api.github.com/organizations/11639138/repos?page=3")
;;         body (:body page)
;;         jsons (json/parse-string body true)]
;;     (swap! raw (fn [old-val new-val] (conj old-val new-val)) body)
;;     (doseq [json jsons]
;;       (swap! repos (fn [old-val new-val] (assoc old-val (:name new-val) (:full_name new-val))) json)))
;;   (let [page (client/get "https://api.github.com/organizations/11639138/repos?page=4")
;;         body (:body page)
;;         jsons (json/parse-string body true)]
;;     (swap! raw (fn [old-val new-val] (conj old-val new-val)) body)
;;     (doseq [json jsons]
;;       (swap! repos (fn [old-val new-val] (assoc old-val (:name new-val) (:full_name new-val))) json))))



