(def +project+ 'miraj/core)
(def +version+ "0.1.0-SNAPSHOT")

;; :checkouts
;; html -> ../../html/html
;; iron -> ../../polymer/iron
;; markup -> ../../markup
;; paper -> ../../polymer/paper

(set-env!

 ;; for testing miraj/assemble, remove src/clj FIXME: move that test to boot-miraj
 :resource-paths #{"src/clj" "resources/public"}
 ;; :source-paths #{"src/test"}

 :checkouts '[[miraj/co-dom "0.1.0-SNAPSHOT"]]

 :dependencies   '[[org.clojure/clojure RELEASE :scope "provided"]
                   ;; [org.clojure/clojurescript "1.9.89"]
                   [org.clojure/data.json "0.2.6"]
                   ;; [org.clojure/core.async "0.2.385"]
                   [org.clojure/tools.namespace "0.2.11" :scope "test"]

                   ;; [clj-http "2.0.0"]
                   [clj-time "0.11.0"]

                   [miraj/co-dom "0.1.0-SNAPSHOT"]

                   ;; testing
;;                   [miraj/html "5.1.0-SNAPSHOT"]
                   [miraj.polymer/paper "1.2.3-SNAPSHOT" :scope "test"]
                   [miraj.polymer/iron "1.2.3-SNAPSHOT" :scope "test"]

                   ;; [mobileink/boot-bowdlerize "0.1.0-SNAPSHOT" :scope "test"]
                   #_[boot/util "RELEASE" :scope "provided"]

                   [stencil "0.5.0"] ;; needed by compiler
                   ;; [cheshire "5.7.0"]

                   ;; used by miraj.http.response
                   [potemkin "0.4.1"]
                   [slingshot "0.12.2"]
                   [ring "1.4.0"]

                   ;; [compojure/compojure "1.4.0" :scope "test"]

                   [org.clojure/tools.logging "0.3.1"]
                   [org.slf4j/slf4j-log4j12 "1.7.1"]
                   [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                      javax.jms/jms
                                                      com.sun.jmdk/jmxtools
                                                      com.sun.jmx/jmxri]]
                   [clj-logging-config "1.9.7"]

                   [boot/core "RELEASE" :scope "test"]
                   [boot/pod "RELEASE" :scope "test"]
                   [miraj/boot-miraj "0.1.0-SNAPSHOT" :scope "test"]
                   ])

(require ;; '[boot-bowdlerize :as b]
         '[miraj.boot-miraj :as miraj])

;; ;; [boot/core "2.5.2" :scope "provided"]
;; ;; [adzerk/boot-test "1.0.7" :scope "test"]
;; [org.slf4j/slf4j-nop "1.7.12" :scope "test"]

;; (require '[adzerk.boot-test :refer [test]])

(task-options!
 aot {:namespace #{'miraj.NSException}}
 repl {:port 8080
       :eval (set-env! :source-paths #(conj % "src/test"))}
 pom  {:project     +project+
       :version     +version+
       :description "miraj core"
       :url         "https://github.com/miraj-project/miraj.core.git"
       :scm         {:url "https://github.com/miraj-project/miraj.core.git"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

;; (deftask asm
;;   "assemble component lib."
;;   []
;;   (set-env! :resource-paths #{"src/test"})
;;   (miraj/assemble :namespace 'foo.bar.baz :pprint true :verbose true))

(deftask dev
  "watch etc."
  []
  (comp (watch)
        (notify :audible true)
        (pom)
        (jar)
        (target)
        (install)))
