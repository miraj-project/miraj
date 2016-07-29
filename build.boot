(def +project+ 'miraj/miraj)
(def +version+ "0.1.0-SNAPSHOT")

(set-env!
 :resource-paths #{"src/clj"}
 ;; :source-paths #{"src/clj"}
 :dependencies   '[[org.clojure/clojure "RELEASE" :scope "provided"]
                   [org.clojure/clojurescript "1.9.89"]
                   [org.clojure/data.json "0.2.6"]
                   [org.clojure/core.async "0.2.385"]
                   [clj-http "2.0.0"]
                   [clj-time "0.11.0"]
                   [miraj/html "5.1.0-SNAPSHOT"]
                   [boot/core "RELEASE" :scope "provided"]
                   [boot/pod "RELEASE" :scope "provided"]
                   [mobileink/boot-bowdlerize "0.1.0-SNAPSHOT" :scope "compile"]
                   #_[boot/util "RELEASE" :scope "provided"]
                   [cheshire "5.5.0"]
                   [ring "1.4.0"]
                   [potemkin "0.4.1"]
                   [slingshot "0.12.2"]
                   [org.clojure/tools.logging "0.3.1"]
                   [org.slf4j/slf4j-log4j12 "1.7.1"]
                   [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                      javax.jms/jms
                                                      com.sun.jmdk/jmxtools
                                                      com.sun.jmx/jmxri]]
                   [clj-logging-config "1.9.7"]
                   ])

;; ;; [boot/core "2.5.2" :scope "provided"]
;; ;; [adzerk/boot-test "1.0.7" :scope "test"]
;; [org.slf4j/slf4j-nop "1.7.12" :scope "test"]

;; (require '[adzerk.boot-test :refer [test]])

(task-options!
 aot {:namespace #{'miraj.NSException}}
 pom  {:project     +project+
       :version     +version+
       :description "miraj core"
       :url         "https://github.com/miraj-project/miraj.core.git"
       :scm         {:url "https://github.com/miraj-project/miraj.core.git"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})
