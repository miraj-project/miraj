(require '[miraj :refer :all])
(co-ns hello.world
  "docstring here"
  (:title "my webpage")
  (:main 'main)
  (:require [miraj.core :refer :all]
            [hello.config :refer :all]
            [polymer.paper :as paper :only [button menu toast toolbar]]
            [polymer.iron :as iron] ;; :refer :all :exclude [behaviors input]]
            [polymer.font :as fnt :only [roboto]]

            ;; [starter-kit.components :only [my-list]]
            ;;FIXME: standard namespaces for scripts and styles?
            [visionmedia.page :js "scripts/lib/page/page.js"]
            [foo.bar :css "styles/myapp/myapp.css"]

            [miraj.html :as h] ;; :refer :all :exclude [button menu progress form]]
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

(println "loading")

;(xml/pprint (:co-fn (meta (find-ns 'miraj.hello))))
;*ns*

(co-fn main [] (h/div
                (paper/card
                 (h/div {:class "card-content"}
                        (h/div "Cards are a convenient means of displaying content composed of different types of objects."
                               (h/br)(h/br)
                               (h/b "Hurray!"))))))

(xml/pprint :html (miraj.hello/main))

(println (xml/serialize :html (activate miraj.hello/main)))


;;(xml/serialize (main))
