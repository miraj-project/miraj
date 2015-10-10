(defproject miraje "1.1.4-SNAPSHOT"
  :description "Miraje - web components, the Clojure way"
  :url "https://github.com/mobileink/polymeraj"
  :license {:name "BSD License"
            :url "http://polymer.github.io/LICENSE.txt"}
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [ clj-logging-config "1.9.7"]
                 ]
  :profiles {:dev {:prep-tasks ^:replace ["clean" "compile"]
                    :source-paths ["src/clj" "src/test" "dev"]
                    }}
  :repl-options {:port 4001})
