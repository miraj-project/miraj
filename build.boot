(def +project+ 'miraj/core)
(def +version+ "0.1.0-SNAPSHOT")

(set-env!

 ;; for testing miraj/assemble, remove src/clj FIXME: move that test to boot-miraj
 :resource-paths #{"src/clj" "resources/public"}
 ;; :source-paths #{"src/test"}

 ;; :checkouts '[[miraj/co-dom "1.0.0-SNAPSHOT"]]

 :dependencies   '[[org.clojure/clojure RELEASE]
                   [org.clojure/data.json "0.2.6"]
                   [clj-time "0.11.0"]
                   [miraj/co-dom "1.0.0-SNAPSHOT"]
                   [stencil "0.5.0"] ;; needed by compiler

                   ;; testing
                   [miraj/html "5.1.0-SNAPSHOT" :scope "test"]
                   ;; [miraj.polymer/paper "1.2.3-SNAPSHOT" :scope "test"]
                   [miraj.polymer/iron "1.2.3-SNAPSHOT" :scope "test"]
                   [pandeiro/boot-http "0.7.3" :scope "test"]

                   ;; [cheshire "5.7.0"]

                   ;; used by miraj.http.response
                   ;; [potemkin "0.4.1"]
                   ;; [slingshot "0.12.2"]
                   ;; [ring "1.4.0"]

                   ;; [compojure/compojure "1.4.0" :scope "test"]

                   [org.clojure/tools.logging "0.3.1"]
                   [org.slf4j/slf4j-log4j12 "1.7.1"]
                   [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                      javax.jms/jms
                                                      com.sun.jmdk/jmxtools
                                                      com.sun.jmx/jmxri]]

                   ;; [boot/core "RELEASE" :scope "test"]
                   ;; [boot/pod "RELEASE" :scope "test"]
                   [adzerk/boot-test "1.2.0" :scope "test"]
                   [miraj/boot-miraj "0.1.0-SNAPSHOT" :scope "test"]])

(require ;; '[boot-bowdlerize :as b]
         '[miraj.boot-miraj :as miraj]
         '[pandeiro.boot-http :refer [serve]]
         '[adzerk.boot-test :refer [test]])

(task-options!
 ;; repl {:port 8080
 ;;       :eval (set-env! :source-paths #(conj % "src/test"))}
 pom  {:project     +project+
       :version     +version+
       :description "miraj core"
       :url         "https://github.com/miraj-project/miraj.core.git"
       :scm         {:url "https://github.com/miraj-project/miraj.core.git"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "build"
  []
  (comp (pom)
        (jar)
        (install)
        (target)))

(deftask dev
  "watch etc."
  []
  (comp (watch)
        (notify :audible true)
        (pom)
        (jar)
        (target)
        (install)))

(deftask devrepl
  "watch etc."
  []
  (comp (cider)
        (repl)
        (watch)
        (notify :audible true)
        (pom)
        (jar)
        (target)
        (install)))

(deftask run-tests
  "compile, link, serve tests"
  []
  ;; (disable-reload! 'boot.user)
  (set-env! :source-paths #(conj % "src/test/clj"))
  (comp
   (build)
   (serve :dir "target") ;; :resource-root "resources")
   (cider)
   (repl)
   (watch)
   (notify :audible true)
   ;; (refresh)
   ;; (demos)
   (target)))
