(ns polymeraj.core
  (:require [hiccup.page :refer [html5]]))

(defmacro co-ns
  [ns docstr reqs & body]
  `(html5
     [:head
      [:title ~docstr]
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport",
              :content
              "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"}]
      [:meta {:name "mobile-web-app-capable", :content "yes"}]
      [:meta {:name "apple-mobile-web-app-capable", :content "yes"}]
      [:script {:src "polymer/webcomponentsjs/webcomponents-lite.js"}]
      [:link {:rel "import", :href "polymer/paper-button/paper-button.html"}]
      [:link {:rel "import", :href "polymer/paper-material/paper-material.html"}]
      ]
     [:body {:id (str '~ns)} ~@body]))

