(ns tools.polymer
  (:refer-clojure :exclude [list map meta time])
  (:require [clojure.tools.logging :as log :only [trace debug error info]]
            [clj-http.client :as client]))

(def raw (atom []))
(def repos (atom {}))

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


