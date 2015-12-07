(require '[miraj :refer :all])
(co-ns hello.world
       "docstring here"
       (:title "my webpage")
       (:main 'main)
       (:polymer [polymer.paper :as paper :only [card]]
                 [polymer.iron :as iron :only [flex-layout]] ;; :refer :all :exclude [behaviors input]]
                 [polymer.font :as fnt :only [roboto]])
       (:html [hello.world :css :refer [core]]
              [hello.world :js :refer [core]]
              [foo.bar :css :refer [baz]]
              [foo.bar :js :refer [baz]])
                 ;;FIXME: also support: :html, maybe :theme?
       (:require [miraj :as miraj :refer :all]
                 [miraj.html :as h]
                 [miraj.http.response :refer [ok created]]
                 [cheshire.core :as json :refer :all]
                 [clojure.data.xml :as xml]
                 [clojure.tools.logging :as log :only [trace debug error info]]
                 [clojure.pprint :as pp]
                 [clojure.string :as string]
                 [clojure.tools.reader :as reader]
                 [clojure.tools.reader.edn :as edn]
                 [clojure.tools.reader.reader-types :as readers]
                 [clojure.test]
                 [clojure.data.xml]
                 [clojure.data.xml.test-utils :refer [test-stream lazy-parse*]]))

(log/trace "loading")

(co-fn main []
       (h/div {:id "cards"}
        (paper/card
         (h/div {:class "card-content"}
                (h/div "Hello, world!"
                       (h/br)(h/br)
                       (h/span "Hoorah!"))))))

;; params
;;   ^:?  a = a is a required query/body param
;;   ^:?? a = a is an optional query/body param
;;   ^:meta uri = uri is a rqst meta-param, e.g. :uri
(defn employee [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "employee " lname status fname)
  (ok (json/generate-string {:lname lname :fname fname :status status :foo foo})))


(defn f1 [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "f1 " lname status fname)
  (ok (json/generate-string {:fn :f1 :lname lname :fname fname :status status :foo foo})))

(defn f2 [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "f2 " lname status fname)
  (ok (json/generate-string {:fn :f2 :lname lname :fname fname :status status :foo foo})))

(defn f1! [^:? lname ^:? fname]
  (log/trace "f2 " lname fname)
  (created (json/generate-string {:fn :f1! :lname lname :fname fname})))

(defn f2! [foo ^:? lname ^:? fname]
  (log/trace "f2 " lname fname)
  (created (json/generate-string {:fn :f2! :foo foo :lname lname :fname fname})))

;; accepted 202, non-authoritative-information 203, no-content 204

(defn dump
  []
  (miraj/dump-dispatch-map))
