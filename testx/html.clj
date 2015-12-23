(ns test.html
  (:require [miraj.core :refer :all]
            [miraj.html.polymer :as h :refer :all]
            [hiccup.page :refer [html5]]
            [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [cljs.core :as cljs]
            [cljs.analyzer :as ana]
            [cljs.compiler :as c]
            [cljs.closure :as cc]
            [cljs.env :as env]))
  ;; (:import [org.jsoup Jsoup Connection Connection$Method]
  ;;          [org.jsoup.nodes Document Document$OutputSettings]))

(log/trace "loading")

;; (log/trace (:name (meta (var my-foo))) (meta (var my-foo)))

;; (pp/pprint my-foo)

(pp/pprint (macroexpand '(h/xform-args (item))))
(pp/pprint (macroexpand '(h/span item)))

(def item "foobar")
item
(instance? clojure.data.xml.Element (h/span))

(xml/pprint (h/span item))
(xml/pprint (h/span {} item))
(xml/pprint (h/span {} (h/span {} item)))
(xml/pprint (h/span {} (h/span "item")))
(xml/pprint (h/span (h/span "item")))

(xml/pprint (h/div (h/span "item")))

(h/pprint (xml/serialize (h/span "item1")
                                  (h/span "item2")))

(xml/pprint
 (xml/serialize (h/span "item1")
                (h/span "item2")))

;; polymer annotations
;; interpret :foo as [[foo]] (one-way binding)
;; what about two-way bindings? ::foo would be nice but won't work because of namespace issues
;; we may have to settle on a convention, e.g. :a+ => {{a}}, or :a% or something
(xml/pprint (h/span :item))            ; default: one-way binding
(xml/pprint (h/span :item%))

(xml/pprint
 (h/div (h/span "item1")
        (h/span "item2")))
(pp/pprint (xml/serialize
 (h/div (h/span "item1")
        (h/span "item2"))))

;; from https://github.com/clojure/data.xml
;; shows that text content is also a list
(xml/pprint (xml/indent-str
 (xml/element :foo {:foo-attr "foo value"}
              (xml/element :bar {:bar-attr "bar value"}
                           (xml/element :baz {} "The baz value1")
                           (xml/element :baz {} "The baz value2")
                           (xml/element :baz {} "The baz value3")))))

(xml/pprint
 (h/span (h/span "item1") (h/span "item2")))

(xml/pprint (xml/indent-str (h/span {} (h/span {} item))))
(xml/pprint (h/div (h/span {} (h/span {} item))))

(xml/pprint (h/div {} (h/span {} (h/span {} item))))
(xml/pprint (h/div (h/span (h/span item))))

(xml/pprint (h/span :author))
(xml/pprint (h/span :author/lname))
(xml/pprint (h/span :author.name/lname))

(xml/pprint (h/span "Author: " :author.name/lname ", " :author.name/fname))

(xml/pprint (h/span {} "item"))
(xml/pprint (h/span (str "item" "one")))
(xml/pprint (h/span itemx))

(xml/pprint (h/span {:class "paper-font-body1"} item))
(xml/pprint (h/span {:class "paper-font-body1" :foo "bar"} "hello world"))
(xml/pprint (h/span {:class "paper-font-body1" :foo "bar"} "hello" "+" "world"))

(xml/pprint (h/span (h/span "foo")))
(xml/pprint (h/span "foo " (h/span "bar") " baz"))
(xml/pprint (h/span "foo " (h/span (h/span "bar")) " baz"))

(xml/pprint (h/span "foo " (h/span {:class "test"} "bar") " baz"))
(xml/pprint (xml/serialize (h/span "foo " (h/span {:class "test"} "bar") " baz")))

(xml/pprint (xml/serialize (h/span {} (h/span (h/span "bar")))))

(xml/pprint
 (xml/serialize
 (h/div (h/span
         "this is an " (h/span {:class "important"} " important") " sentence."))))
(xml/pprint
 (xml/serialize
 (h/div (h/span
         "this is a " (h/span {:class "important"} (h/i "highly") " important") " sentence."))))

(xml/pprint (h/span "foo" "baz"))
(xml/pprint (h/span (str "foo" "baz")))
(xml/pprint (h/div (h/span "foo")
                 (h/span "bar")))

(xml/pprint (h/span "foo " (h/span "bar")))
(xml/pprint (h/span (str "foo " (h/span "bar"))))
(xml/pprint (h/span (list "foo " (h/span "bar") "baz")))

(xml/pprint (h/span (str "foo " (h/span "bar") " <baz")))

(xml/pprint (h/span "foo " (h/span {:class "test"} "bar") " baz"))
(xml/pprint (h/span :foo (h/span {:class "test"} :bar) :baz))

(xml/pprint
 (h/div (h/span
         (str "this is a " (h/span {:class "important"} (h/i "highly") " important") " sentence."))))

(xml/pprint
 (h/div (h/span
            "this is a " (h/span {:class "important"} (h/i "highly") " important") " sentence.")))

(xml/pprint (h/li (h/span {:class "paper-font-body1"} "foo " (h/span item) " baz")))

(xml/pprint (h/li (h/span {:class "paper-font-body1"} "foo")))

(xml/pprint
 (xml/serialize
  (h/ul {:foo 0 :bar 1}
        (h/li (h/span {:class "paper-font-body1"} "item1"))
        (h/li (h/span {:class "paper-font-body2"} "item2"))
        (h/li (h/span {:class "paper-font-body3"} "item3"))
        )))

(xml/pprint
 (h/ul
  (h/template {:is "dom-repeat" :items :items}
              (h/li (h/span {:class "paper-font-body1"} :item)))))

(xml/pprint ;; (xml/pprint-html
          (h/div "hello")
          (h/div "world"))

(xml/pprint
 (xml/serialize
                (h/div
                 (for [y (range 1 4)]
                   (do (log/trace "TURN " y)
                       (h/span "hello" y))))))
(xml/pprint
 (xml/serialize :with-xml-declaration
                (h/div
                 (for [y (range 1 4)]
                   (do (log/trace "TURN " y)
                       (h/span "hello" y))))))
(xml/pprint
 (xml/serialize :html
                (h/div
                 (for [y (range 1 4)]
                   (do (log/trace "TURN " y)
                       (h/span "hello" y))))))

(xml/pprint
 (xml/serialize
  (h/div
   (for [y (range 1 4)]
     (do ;; (log/trace "TURN " y)
         (h/span "hello" y))))))

(xml/pprint
 (h/div
  {:class "my-list"}
  (h/ul
   (clojure.core/map
               #(h/li {} (h/span {:class "my-list-item"}  %))
               ["item1" "item2"]))))

(xml/pprint (h/ol (clojure.core/map #(h/li (h/div {:class "my-list-item"}  %)) ["item1" "item2"])))
=> ("<li><div class=\"my-list-item\">item1</div></li> <li><div class=\"my-list-item\">item2</div></li>")
(xml/pprint (h/ol (clojure.core/map #(h/li (h/div {:class "my-list-item"}  %)) ["item1" "item2"])))
=> (<li><div class="my-list-item">item1</div></li> <li><div class="my-list-item">item2</div></li>)

(xml/pprint (h/ul (clojure.core/map #(h/li (h/div {:class "my-list-item"}  %)) ["item1" "item2"])))
=> clojure.lang.LazySeq

(xml/pprint
 (h/div
  (into '() (for [x (range 1 4)]
              (do (log/trace "TURN " x)
                  (h/span x))))))

(def x 99)
(xml/pprint
 (h/ul
 (merge '() (string/join " "
                        (for [i (list "hello" "world" x)]
                                          i)))))

;)

(xml/pprint
 (xml/serialize
  (h/dom-module {:id "my-foo"}
                (h/template
                 (h/style {:include "shared-styles"})
                 (h/ul {}
                  (h/template {:is "dom-repeat" :items "{{items}}"}
                              (h/li {} (h/span {:class "paper-font-body1"} :item))
                              (h/li {} (h/span {:class "paper-font-body2"} (keyword (str "item" 2))))
                              (h/li (h/span {:class "paper-font-body3"}"{{item}}"))))))))

(xml/pprint
 (xml/serialize
  (h/html
    (h/link {:rel "import" :href "polymer/polymer/polymer.html"})
    (h/link {:rel "import" :href "styles/shared/style-modules.html"})
    (h/dom-module {:id "my-foo"}
      (h/template
        ;; (h/style {:include "shared-styles"})
        (h/ul
          (h/template {:is "dom-repeat" :items "{{items}}"}
            (h/li (h/span {:class "paper-font-body1"}"{{item}}"))
            (h/li {:id "special"} (h/span {:class "paper-font-body2"}"{{item}}"))
            (h/li (h/span {:class "paper-font-body3"}
                          "here's some " (h/i "important") " news!"))
            (h/li (h/span {:class "paper-font-body1"}"{{item}}")))))))))

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
;; (let [form (read1 "(foo 1)")]
;;   (pp/pprint (ana/analyze user-env form)))

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

;; we use "co-type" rather than "web-component" since we use the (html) constructor
;; to instantiate the co-type (construct tokens of the co-type)
;; the point is to emphasize typeness and the token:type (a:A) relation
;; "web-component" isn't so good since it really only applies to tokens
(co-type my-list
  "doc string here"
  ;; type is my-list; ctor must have same name? if so we can just use default "ctor"?
  ;; ctor must have same name as co-type. we cannot have variant ctors for the co-type,
  ;; since the ctor generates the html tag defn and we only get one of those.
  ;; but note that we can parameterize so every instantiation may be different
  (:require  ;; FIXME: link, script, etc. is metadata!
   [polymer]
   [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
   [styles.shared :refer [foo-styles bar-styles]]
   ;; => <link rel="import" href="styles/shared/style-modules.html">
   ;; => plus <style include="...style module..."> in module template
   ;; => or <style is="custom-style" include="...style module..."></style>
  (ctor
  ;; (my-list
    "ctor doc string here"
    ;; parameters determine properties:
    [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
     author
     ^{:type Content, :default nil}
     content]
    (h/dom-module  ;; ID = ctor string, added automatically
      (h/template
       ;; "scoped" styles:
       (h/style {:include "shared-styles"})
       (h/style (css ...)) ;; use https://github.com/noprompt/garden ?
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

;;
(co-type my-widget
  "doc string here"
 (ctor
  [author content items]
    (h/dom-module  ;; ID = ctor string, added automatically
      (h/template
        (h/style {:include "shared-styles"})
        (h/ul
         (let [items [1 2 3]]
          (h/for [item items]
            (h/li (h/span {:class "paper-font-body1"} item))
            ))))))))


;; alternative ctor defn:

(ns-unmap *ns* 'item)

(pp/pprint (xml/serialize
;; <link rel="import" href="../../polymer/polymer/polymer.html">
 (co-type my-widget
          "docstring"
          (:require [polymer]  ;; "polymer/polymer/polymer.html"
                    ;; => <link rel="import" href="polymer/polymer/polymer.html">
                    [styles.shared.style-modules] ;; "styles/shared/style-modules.html"]
                    [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
                    ;; (h/style {:include "shared-styles"}) - goes inside template elt
                    [css.foo :css ["foo.css"]]
                    [js.foo :js ["foo.js"]])
          (ctor
           ;; parameters/properties
           [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
            author
            list-items ;; make arg to dom-repeat explicit
            ^{:type Content, :default nil}
            content]
           (h/div
            (h/link {:rel "import" :href "/polymer/polymer/polymer.html"})
            (h/ul
            (h/li (h/span {:class "paper-font-body1"} :author))
            (h/li (h/span {:class "paper-font-body1"} :author%))))
          ))))

(xml/pprint
 (co-type my-widget
          "docstring"
          (:require [polymer])
          (:export [editor author])
          (ctor
           (h/ul
            (h/li (h/span {:class "paper-font-body1"} :author/fname ", " :editor%))))
          ))

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
