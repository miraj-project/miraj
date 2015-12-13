(defproject miraj "1.1.4-SNAPSHOT"
  :description "Miraje - web components, the Clojure way"
  :url "https://github.com/mobileink/polymeraj"
  :license {:name "BSD License"
            :url "http://polymer.github.io/LICENSE.txt"}
  :source-paths ["src/clj"]
  :resource-paths ["resources/public"]
  ;; :aot [miraj]
  :dependencies [[org.clojure/clojure "1.8.0-RC3"] ;; "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 ;; [org.clojure/core.async "0.2.375-HACK"]
                 [org.clojure/clojurescript "1.7.170"]
                 ;; [org.clojure/tools.analyzer.jvm "0.6.8"]
                 [org.clojure/tools.namespace "0.2.11"]
                 ;; [org.clojure/data.xml "0.0.8"]
                 [miraj/markup "0.1.0-SNAPSHOT"]
                 [clj-http "2.0.0"]
                 [cheshire "5.5.0"]
                 [ring/ring-core "1.4.0"]
                 [potemkin "0.4.1"]
                 [slingshot "0.12.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [clj-logging-config "1.9.7"]
                 ]
  :profiles {:dev {:prep-tasks ^:replace ["clean" "compile"]
                   :source-paths ["src/clj" "test" "startup"]
                   :dependencies [[ring "1.4.0"]
                                  [potemkin "0.4.1"]
                                  [polymer/iron "1.2.3-SNAPSHOT"]
                                  ;;[org.clojure/tools.namespace "0.3.0-alpha2"]
                                  ]
                    }}
  :repl-options {:port 4001}
  :test-selectors {:polymer :polymer})
