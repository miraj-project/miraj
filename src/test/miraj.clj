(ns test.miraj
  (:require [miraj.core :refer :all]
            [miraj.html :as h :refer :all]
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

(println "loading test.miraje")
(log/trace "trace loading test.miraje")


(co-ns
  "dashboard"
  (:require [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
            ;; => link href="polymer/iron-list/iron-list.html", etc.
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

(println (h/li (h/span {:class "paper-font-body1"}"{{item}}")))

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
                    (str "here's some " (h/i "important") " news!")))
            (h/li (h/span {:class "paper-font-body1"}"{{item}}")))))))

(first (read1 "(for [item items] (h/li item))"))

;; (let [form (read1 "(for [item items] (h/li item))")] ;; "(if x true false)")]
;;   (pp/pprint (ana/parse (first form) user-env form nil nil)))

;; (let [form (read1 "(for [item items] (h/li item))")] ;; "(if x true false)")]
;;   (pp/pprint (ana/analyze user-env form)))

(let [form (read1 "(if x true false)")]
  (pp/pprint (ana/parse (first form) user-env form nil nil)))

(let [form (read1 "(foo 1)")]
  (pp/pprint (ana/analyze user-env form)))


(cljs->js (if x true false))

(cljs->js (fn [a b] (+ a b)))

(cljs->js (foo 1))

(cljs->js (_typeChanged [type]
            (this-as me
              )))

(cljs->js (defn _typeChanged [type]
            (this-as me
              )))


(let [form (read1 "(fn [a b] (+ a b))")]
  (with-out-str (c/emit (ana/analyze user-env form))))
(let [form (read1 "(foo 1)")]
  (pp/pprint (ana/analyze user-env form)))

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


(cljs->js (fn []))

(let [thing "items"]
  (println (h/li thing)))

(let [li "items"]
  (str "foo " li " bar"))

(ctor->template
 (my-list
    [author content]
    (h/dom-module  ;; ID = ctor string, added automatically
      (h/template
        (h/style {:include "shared-styles"})
        (h/ul
          (dom-repeat [item items]
            (h/li (h/span {:class "paper-font-body1"} item))
            ))))))


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


;; alternative ctor defn:

(ctor "my-list"
  "MyList value constructor"
  ;; instead of a bunch of [:link...] decls we use metadata:
  ^{:imports ["polymer/polymer/polymer.html"
              "styles/shared/style-modules.html"]
    ;; (h/style {:include "shared-styles"}) - goes inside template elt
    :css ["foo.css"]
    :js ["foo.js"]}
    [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
     author
     list-items ;; make arg to dom-repeat explicit
     ^{:type Content, :default nil}
     content]
    ;; no need for explicit dom-module, nor immediate template
    (h/ul
      (for [item list-items]  ;; instead of <template is="dom-repeat"...>
        ;; TODO: make index explicit using std clojure
        (seq  ;; FIXME: do we need this?
          (h/li (h/span {:class "paper-font-body1"} item))
          (h/li :#special (h/span {:class "paper-font-body2"} item))
          (h/li (h/span {:class "paper-font-body3"} item)))
        )))

;; a ctor expr will emit a dom-module string.  we want to be able to
;; use any and all of clojure for defining the ctor, while
;; metaprogramming the html/js/css.

;; For example, we might use (for [item items]...) to generate a
;; static list, using some data defined outside of the ctor defn.  the
;; result will be a hardcoded ul containing some li elements.  but we
;; also want to use ordinary clojure to express things like
;; dom-repeat.  The trick here is to detect use of formal args.  so if
;; list-items is listed as a formal arg to the ctor, and we detect
;; that it is used in a for clause, eg. (for [item list-items]...) we
;; know that the dom-repeat is indicated.  as a double check, "item"
;; is also required in dom-repeat.

;; in short, we determine at compile time that some clojure
;; expressions should be interpreted as "co-expressions", and
;; translated into Polymer.  iow we treat clojure as polysemous; its
;; expressions only ever have meaning "under an interpretation".  that
;; interpretation is almost always the standard interpretation, but we
;; can use meta-programming to enable alternative interpretations.


;; dom-repeat example from
;; https://www.polymer-project.org/1.0/docs/devguide/templates.html#dom-repeat

;; <dom-module id="employee-list">
;;   <template>

;;     <div> Employee list: </div>
;;     <template is="dom-repeat" items="{{employees}}">
;;         <div># <span>{{index}}</span></div>
;;         <div>First name: <span>{{item.first}}</span></div>
;;         <div>Last name: <span>{{item.last}}</span></div>
;;     </template>

;;   </template>
