;;(require 'miraj)
;;(println
;; (macroexpand
(miraj/co-ns co-ns.meta-test
             "docstring here"
             (:main index)
             (:title "meta test")
             (:meta {:description "Miraj Hello World demo"
                     :application-name "hello-world"})
             (:apple {:mobile-web-app {:capable true
                                       :status-bar-style :black
                                       :title "Hello"}})
                     ;; apple-touch-icon is a <link> thing, not <meta>
                             ;; :touch-icon "images/touch/apple-touch-icon.png"}
             (:msapp {:navbutton-color "#FFCCAA"
                      :tile-color "#3372DF"
                      :tile-image "images/ms-touch-icon-144x144-precomposed.png"})
             (:mobile {:agent {:format :html5
                               :url "http://3g.sina.com.cn/"}
                       :web-app-capable true
                       :theme-color "#2E3AA1"})
             ;; (:scripts [foo.bar :refer [baz]])
             ;; (:styles [foo.bar :refer [baz]])
             )

             ;; (:polymer [polymer.iron :as iron]
             ;;           [polymer.paper :as paper])
             ;; (:require [miraj.html :as h]
             ;;           [clojure.pprint :as pp]))

(miraj.markup/pprint
 :html
 (:co-config (meta (find-ns 'co-ns.meta-test))))

;; (println (h/span))

;; (println (paper/button))

;; (println (iron/list))

;; (clojure.pprint/pprint
;;  (macroexpand
;;   '(ns miraj
;;     (:require [miraj.html :as h]
;;               [clojure.pprint :as pp]))))

