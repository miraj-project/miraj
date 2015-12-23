(ns test.co-types
  (:require [miraj.core :refer :all]
            [miraj.html.polymer :as h :refer :all]
            [hiccup.page :refer [html5]]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.core :as cljs]
            [cljs.analyzer :as ana]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env])
  (:import [java.io StringReader]))

(log/trace "loading")

(co-ns
  "dashboard"
  (:require [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
            [polymer.paper :as paper :refer [button drawer-panel icon-button item
	    		       	     	     material menu scroll-header-panel
					     spinner styles toolbar]]
            ;; the nasty html/js way - note the typo, namespace is ignored:
            ;; [dashboard.components :html  "components/my-greeting/my-greeting.html"]
            ;; the cool clojure/clojurescript way:
            [dashboard.components :refer [my-list my-greeting]]
            [visionmedia.page :js "scripts/lib/page/page.js"]
            ;; [theme.iron-list :html "styles/lib/page/page.js"]
            ))

(co-defn my-foo
  []
  [:link {:rel "import" :href "polymer/polymer/polymer.html"}]
  [:link {:rel "import" :href "styles/shared/style-modules.html"}]
  [:dom-module#my-foo
   [:template
    [:style {:include "shared-styles"}]
    [:ul
     [:template {:is "dom-repeat" :items "{{items}}"}
      [:li [:span {:class "paper-font-body1"}"{{item}}"]]]]]])

;; (log/trace (:name (meta (var my-foo))) (meta (var my-foo)))

;; (pp/pprint my-foo)

(println (h/span {:class "paper-font-body1"}"{{item}}"))

(println (h/span {:class "paper-font-body1"}
           "this is an " (h/i "important") " sentence."))

(println (h/li "foo"))

(println (h/li (h/span {:class "paper-font-body1"}
                       "{{item}}")))

(println (h/ul {:foo 0 :bar 1}
           (h/li (h/span {:class "paper-font-body1"}"{{item}}"))
           (h/li (h/span {:class "paper-font-body2"}"{{item}}"))
           (h/li (h/span {:class "paper-font-body3"}"{{item}}"))
           ))

(println (h/ul
           (h/template {:is "dom-repeat" :items "{{items}}"}
             (h/li (h/span {:class "paper-font-body1"}"{{item}}")))))

(println
  (h/dom-module {:id "my-foo"}
    (h/template
      (h/style {:include "shared-styles"})
      (h/ul
        (h/template {:is "dom-repeat" :items "{{items}}"}
          (h/li (h/span {:class "paper-font-body1"}"{{item}}"))
          (h/li (h/span {:class "paper-font-body2"}"{{item}}"))
          (h/li (h/span {:class "paper-font-body3"}"{{item}}")))))))

(println
    (h/link {:rel "import" :href "polymer/polymer/polymer.html"})
    (h/link {:rel "import" :href "styles/shared/style-modules.html"})
    (h/dom-module {:id "my-foo"}
      (h/template
        (h/style {:include "shared-styles"})
        (h/ul
          (h/template {:is "dom-repeat" :items "{{items}}"}
            (h/li (h/span {:class "paper-font-body1"}"{{item}}"))
            (h/li {:id "special"} (h/span {:class "paper-font-body2"}"{{item}}"))
            (h/li (h/span {:class "paper-font-body3"}
                    (str "here's some " (h/i "important")
                         " " (h/span "{{item}}") " news!")))
            (h/li (h/span {:class "paper-font-body4"}"{{item}}")))))))


;; from iron-meta:
;;       _typeChanged: function(type) {
;;         this._unregisterKey(this.key);
;;         if (!metaDatas[type]) {
;;           metaDatas[type] = {};
;;         }
;;         this._metaData = metaDatas[type];
;;         if (!metaArrays[type]) {
;;           metaArrays[type] = [];
;;         }
;;         this.list = metaArrays[type];
;;         this._registerKeyValue(this.key, this.value);
;;       },


    ;; [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
    ;;  author
    ;;  ^{:type Content, :default nil}
    ;;  content]

(println (dom-repeat [thing things]
           (h/li "thing")
           (h/li "thing"))
  )

(let [thing "items"]
  (println (h/li thing)))

(let [li "items"]
  (str "foo " li " bar"))

(co-type my-list
  "doc string here"
  (my-list  ;; ctor -> dom-module
    "ctor doc string here"
    [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
     author
     ^{:type Content, :default nil}
     content]
    ;; FIXME: link, script, etc. is metadata!
    (h/link {:rel "import" :href "polymer/polymer/polymer.html"})
    (h/link {:rel "import" :href "styles/shared/style-modules.html"})
    (h/dom-module  ;; ID = ctor string, added automatically
      (h/template
        (h/style {:include "shared-styles"})
        (h/ul
          (h/template {:is "dom-repeat" :items "{{items}}"}
            (h/li (h/span {:class "paper-font-body1"}"{{item}}"))
            (h/li {:id "special"} (h/span {:class "paper-font-body2"}"{{item}}"))
            (h/li (h/span {:class "paper-font-body3"}"{{item}}"))
            )))))

  MutationObservation ;; protocol based on MutationObserver http://www.w3.org/TR/dom/#mutationobserver
  (author (fn [e] ...)) ;; instead of "observed" attr on "author" prop descriptor

  Polymer.Gesture ;; gesture events protocol
  (down (fn [this e] ...))
  (up (fn [this e] ...))
  (tap (fn [this e] ...)) ;; registers handler fn as listener for tap on the my-list elt
  (tap (fn [#special e] ...)) ;; respond to tap event on elt with id "special"
  (track (fn [e] ...))

  DOM.Events ;; protocol

  ;; behaviors are protocols
  Polymer.PaperButtonBehavior
  (active ...)
  (disabled ...)
  (toggle ...)
  (addOwnKeyBinding ...)
  (active ...)
  )

(defmacro co-type  ;; or:  defwebtype?
  [nm ctor & proto+mmaps]
  ;; 1.  convert ctor to HTML + prototype map
  ;; 2.  recur over proto+mmaps using cljs->js to get method def strings
  ;; 3.  construct <dom-module> elt
  ;; 4.  alter-var-root to attach fn returning dom-module

  (let [ns *ns*
        ctor (analyze-ctor ctor)  ;; convert ctor expr to js prop map + html
        proto+mmap (partition 2 proto+mmaps)]
    `(let [n# (def ~nm)] ;; ~args ~@body)]
           val# (alter-var-root
                      var#
                      (fn [oldval#]
                        [:head
                         [:title ~docstr]
                         [:meta {:charset "utf-8"}]
                         [:meta {:name "description" :content ~docstr}]

                         [:meta {:name "viewport",
                                 :content
                                 "width=device-width, minimum-scale=1.0, initial-scale=1, user-scalable=yes"}]

                         ;; Web Application Manifest
                         [:link {:rel "manifest" :href "manifest.json"}]

                         ;; Chrome for Android theme color
                         [:meta {:name "theme-color" :content "#2E3AA1"}]
                         ;; Add to homescreen for Chrome on Android
                         [:meta {:name "mobile-web-app-capable" :content "yes"}]
                         [:meta {:name "application-name" :content "PSK"}]
                         [:link {:rel "icon" :sizes "192x192"
                                 :href "images/touch/chrome-touch-icon-192x192.png"}]

                         ;; Add to homescreen for Safari on iOS
                         [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
                         [:meta {:name "apple-mobile-web-app-status-bar-style" :content "black"}]
                         [:meta {:name "apple-mobile-web-app-title" :content (str ~docstr)}]
                         [:link {:rel "apple-touch-icon" :href "images/touch/apple-touch-icon.png"}]

                         ;; Tile color for Win8
                         [:meta {:name "msapplication-TileColor" :content "#3372DF"}]
                         ;; Tile icon for Win8 (144x144)
                         [:meta {:name "msapplication-TileImage"
                                 :content "images/touch/ms-touch-icon-144x144-precomposed.png"}]

                         ;; Conventions
                         [:link {:rel "stylesheet"
                                 :href (str "styles/" (ns-to-path ~ns) ".css")}]
                         [:link {:rel "import"
                                 :href (str "themes/" (ns-to-path ~ns) ".html")}]
                         ;; :href "styles/panels-theme.html"}] ;; {{project}}.css
                         [:link {:rel "import" :href "styles/shared/style_modules.html"}]
                         [:style {:is "custom-style" :include "shared-styles"}]
                         [:script {:src "polymer/webcomponentsjs/webcomponents-lite.js"}]
                         ~@reqs
                         ]))]
           var#))

