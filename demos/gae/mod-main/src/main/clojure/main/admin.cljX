(ns main.admin
  ;; (:import [com.google.appengine.api.datastore
  ;;           DatastoreService
  ;;           FetchOptions$Builder
  ;;           Query
  ;;           Query$Filter])
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log :only [trace debug info]]
            [compojure.api.sweet :refer :all]
            ;; [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.handler.dump :refer :all] ; ring-devel
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.file-info :refer [wrap-file-info]]
            [ring.util.http-response :refer :all]
            [ring.util.response :as r]
            [ring.util.servlet :as servlet]
            ))

(println "loading mod-main: main.admin")
;
(defn admin-docs []
  (swagger-docs
   "/admin/swagger.json"
   {:info {:title "Main Admin API"
           :description "Admin API for main backend"}
    :tags [{:name "device", :description "Device Administration"}
           {:name "user", :description "User Administration"}]}))

(defapi admin-api
  (swagger-ui "/admin"
              :swagger-docs "/admin/swagger.json")
  (admin-docs)

  {:formats [:edn]}

  (context* "/admin" []
            :tags ["device"]

    (GET* "/widgets" []
          :summary "administer widgets"
;;          :body [...]
          :return String
          (log/trace "GET* widgets")
          (ok (str (format "<h1>Widgets</h1>")
                   "\n\n<a href='/'>blah blah</a>")))

    (GET* "/gadgets" []
          :summary "administer gadgets"
          (log/trace "GET* gadgets")
          (str (format "<h1>Gadgets</h1>")
               "\n\n<a href='/'>blah blah</a>")))

  (context* "/user" []
    :tags ["user"]

    (GET* "/registrants" []
          :summary "list registered users"
         (str (format "<h1>REGISTRANTS</h1>")
              "\n\n<a href='/'>blah blah</a>"))

    (GET* "/:user" [user]
          :summary "get info about user"
          (ok (str (format "<h1>HI, %s</h1>" user))))

    (route/not-found "<h1>Page not found</h1>")))

(servlet/defservice
  (-> (routes admin-api)
      wrap-params
      wrap-file-info
      ))


