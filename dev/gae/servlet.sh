SRC=./mod-main/src/main/clojure/
EAR=ear/build/exploded-app/mod-main-0.1.0/WEB-INF/classes
MOD=mod-main/build/classes/main

mkdir -p $MOD

java -cp .:/Users/gar/.m2/repository/org/clojure/clojure/1.8.0-RC3/clojure-1.8.0-RC3.jar:/Users/gar/.m2/repository/javax/servlet/javax.servlet-api/3.1.0/javax.servlet-api-3.1.0.jar:$SRC -Dclojure.compile.path=$MOD clojure.lang.Compile hello.servlets
