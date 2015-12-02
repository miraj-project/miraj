;;(co-ns miraj.test-webpage
(ns miraj.test-webpage
  (:require [miraj.core :refer :all]
            [miraj.polymer.paper :as paper :refer :all]
            [miraj.html :as h :refer :all :exclude [button input menu progress]]
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

(xml/serialize (paper/menu {:foo :bar}))
(xml/serialize (h/menu))

(co-ns ;; foo.bar
  "Miraj Starter Kit"
  (:require [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
            [polymer.paper :as paper :refer [button drawer-panel icon-button item
                                             material menu scroll-header-panel
                                             styles toolbar]]
            ;; the nasty html/js way - note the typo, namespace is ignored:
            [starter-ket.components :html  "components/my-greeting/my-greeting.html"]
            ;; the cool clojure/clojurescript way:
            [starter-kit.components :refer [my-list]]
            [visionmedia.page :js "scripts/lib/page/page.js"]
            ))


;; Miraj

;; (h/body
;;  (h/div
;;   (p/paper-button "hello")))

