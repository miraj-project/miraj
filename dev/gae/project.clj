(defproject gae "0.0.1-SNAPSHOT"
  :source-paths ["mod-main/src/main/clojure" "checkouts/miraj/src/clj"]
  :resource {
             :resource-paths [["mod-main/src/main/clojure"
                              {:target-path "ear/build/exploded-app/mod-main-0.1.0/WEB-INF/classes"
                               :includes [ #".*.clj" ]
                               :excludes [ #".*~" ]}]
                              ["checkouts/miraj/src/clj"
                              {:target-path "ear/build/exploded-app/mod-main-0.1.0/WEB-INF/classes"
                               :includes [ #".*.clj" ]
                               :excludes [ #".*~" ]}]]
             :update false      ;; if true only process files with src newer than dest
             :silent false
             :verbose false
             :skip-stencil [ #".*" ]
             }
  ;; :plugins [[lein-auto "0.1.2"]
  ;;           [lein-resource "15.10.1"]])
)
