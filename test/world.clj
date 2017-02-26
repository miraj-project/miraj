
;; OBSOLETE: we use defpage, not co-ns, etc.

(println "starting load of hello/world.clj")
(miraj/co-ns hello.world
       "docstring here"
       (:main index)
       (:meta :description "Miraj Hello World demo"
              :title "Hello World!"
              ;; global attrs: http://www.w3.org/html/wg/drafts/html/master/dom.html#global-attributes
              ;; meta standard names: :application-name, :author, :description, :generator, :keywords
              ;; :charset;  :itemprop?
              ;; extensions: https://wiki.whatwg.org/wiki/MetaExtensions
              ;; see https://gist.github.com/kevinSuttle/1997924
              ;; :viewport gets special treatment
              ;; e.g.  :dc {:created ".." :creator "foo" ...}
              :application-name "hello-world"
              :apple {:mobile-capable :true
                      :status-bar-style "black"
                      :title "Hello"
                      :touch-icon "images/touch/apple-touch-icon.png"}
              :msapplication {:navbutton-color "..."
                              :tile-color "#3372DF"
                              :tile-image "images/ms-touch-icon-144x144-precomposed.png"}
              :mobile-capable true
              :theme-color "#2E3AA1")
       (:base ..) http://www.w3.org/html/wg/drafts/html/master/semantics.html#the-base-element
       ;; :href, :target
       (:aria ..)
       ;; link elts must have :rel and :href
       ;; instead of rel="foo" href="bar", we do {:foo "bar"}
       (:link {:author "http://foo"})
       (:nav ..) ; :first, :last, :prev, :next (link rel vals)
       (:icon {:sizes "192x192" :href="images/chrome-touch-icon-192x192.png"})
       (:viewport ...) ;; :width, :height, initial-scale, minimum-scale, maximum-scale, user-scalable
       (:pragma ;; http://www.w3.org/html/wg/drafts/html/master/semantics.html#pragma-directives
        ;; = http-equiv attrib on meta elt
        ;; standard: :content-language, :content-type, :default-style, :refresh, :set-cookie,
        ;;     :x-ua-compatible, :Content-Security-Policy
        ;; extensions:  https://wiki.whatwg.org/wiki/PragmaExtensions
        )
       (:polymer [polymer.paper :as paper :only [card]]
                 [polymer.iron :as iron :only [flex-layout]]
                 [polymer.font :as fnt :only [roboto]])
       (:styles [hello.world :refer [core]]) ;; :refer or :use? or either?
       (:scripts [hello.world :refer [core]])

       (:require [miraj :as miraj :refer :all]
                 [miraj.html :as h]
                 [miraj.http.response :refer [ok created]]
                 [cheshire.core :as json :refer :all]
                 [clojure.tools.logging :as log :only [trace debug error info]]
                 [clojure.pprint :as pp]
                 [clojure.string :as string]))

(log/trace "loading")
;;(println (meta hello.world))
(miraj/co-fn index []
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
  (ok (miraj/dump-dispatch-map)))
