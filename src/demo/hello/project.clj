(defproject miraj-test "0.0.1-SNAPSHOT"
  :description "Miraj - web components, the Clojure way"
  :url "https://github.com/mobileink/miraj"
  :license {:name "BSD License"
            :url "http://polymer.github.io/LICENSE.txt"}
  :source-paths ["src/clj"]
  :resource-paths ["resources/public"]
  :dependencies [[org.clojure/clojure "1.8.0-RC3"] ;; "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/clojurescript "1.7.170"]
                 [mobileink/data.xml "0.0.10-SNAPSHOT"]
                 [miraj "1.1.4-SNAPSHOT"]
                 [cheshire "5.5.0"]
                 [compojure "1.4.0"]
                 [ring/ring-core "1.4.0"]
		 [ring/ring-defaults "0.1.5"]
                 [ring/ring-headers "0.1.3"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [clj-logging-config "1.9.7"]
                 ]
  :profiles {:dev {:prep-tasks ^:replace ["clean" "compile"]
                   :source-paths ["src/clj" "test" "dev"]
                   :resource-paths ["resources/public"]
                   :dependencies [[ring "1.4.0"]
                                  [potemkin "0.4.1"]
                                  [slingshot "0.12.2"]
                                  [org.clojure/tools.namespace "0.2.11"]]
                    }}
  :repl-options {:port 4001}
  :plugins [[lein-ring "0.8.13"]
            [lein-cljsbuild "1.1.0"]]
  :ring {:handler miraj/start :port 8087}
  :test-selectors {:polymer :polymer})
