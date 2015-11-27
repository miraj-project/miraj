(ns test.ctors
  (:require [miraj.core :refer :all]
            [miraj.html :as h :refer :all]
            [hiccup.page :refer [html5]]
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

(log/trace "loading")

;; example 1. https://www.polymer-project.org/1.0/docs/devguide/data-binding.html

;; <dom-module id="host-element">
;;     <template>
;;       <child-element name="{{myName}}"></child-element>
;;     </template>
;; </dom-module>

;; this example assumes that 'child-element' has been defined in some
;; namespace.  we mimic this using generic h/elt
(defmacro child-element [& args] `(h/elt :child-element ~@args))
(println (ctor->template
          (host-element
           [^% my-name]
           (child-element {:name my-name}))))

(println (ctor->template
          (host-element
           [^% my-name]
           (h/span {:id "foo"} "hi world"))))

(println (ctor->template
          (host-element
           [^% my-name]
           (for [x (range 1 4)]
             (h/div (string/join " " (for [i (list "hello" "world" x)]
                                       i)))))))

;;;;  for-dom

;; basics
(println (pretty-print-html
          (ctor->template
           (my-list
            [author content]
            (for-dom [item list-items]
                      (h/li (h/span item)))))))

(println (pretty-print-html
          (ctor->template
           (my-list
            [author content]
            (for-dom [item list-items]
                      (h/li {:foo item} "bar"))))))

;; paths, using get-in
;; assume list items are maps of form {:item {:name {:first "Joe" :last "Public"}}}
(let [item {:name {:first "Joe" :last "Public"}}]
  (println (pretty-print-html
            (ctor->template
             (my-list
              [author content]
              ;; (log/trace "item: " item)
              (h/span "foo" "bar" (get-in item [:name :first])))))))

;; better: instead of (get-in item [:name :first]), use (-> item :name :first)


(let [item {:name {:first "Joe" :last "Public"}}]
  (println (pretty-print-html
            (ctor->template
             (my-list
              [author content]
              ;; (log/trace "item: " item)
              (h/span (string/join " "
                                   ["foo" "bar" (get-in item [:name :first])])))))))

(println (pretty-print-html
          (ctor->template
           (my-list
            [author content]
            (for-dom [item list-items]
                      (h/li (h/span (get-in item [:name :first]))))))))

(println ;;(h/pretty-print-html
 (ctor->template
                    (my-list
                     [^% author content item]
                     (h/ul
                      (for-dom [[index ^% item] ^% list-items]
                               (h/li {:auth  author
                                      :class$ "author"}
                                     (h/span index) ". "
                                     (h/span {:class "paper-font-body1" :author item}
                                             "foo2 " (h/span item) " bar2")))))))

(println (pretty-print-html (ctor->template
                    (my-list
                     [^% author content item]
                     (h/ul
                      (for-dom [[index ^% item] ^% list-items]
                               (h/li {:auth  author
                                      :class$ "author"}
                                     (h/span index) ". "
                                     (h/span {:class "paper-font-body1" :author item}
                                             "foo2 " (h/span item) " bar2"))))))))


;; alternative ctor defn:

;; (ctor "my-list"
;;   "MyList value constructor"
;;   ;; instead of a bunch of [:link...] decls we use metadata:
;;   ^{:imports ["polymer/polymer/polymer.html"
;;               "styles/shared/style-modules.html"]
;;     ;; (h/style {:include "shared-styles"}) - goes inside template elt
;;     :css ["foo.css"]
;;     :js ["foo.js"]}
;;     [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
;;      ^% ;; 2-way binding
;;      author
;;      list-items ;; make arg to dom-repeat explicit
;;      ^{:type Content, :default nil}
;;      content]
;;     ;; no need for explicit dom-module, nor immediate template
;;     (h/ul
;;       (for [item list-items]  ;; instead of <template is="dom-repeat"...>
;;         ;; TODO: make index explicit using std clojure
;;         (seq  ;; FIXME: do we need this?
;;           (h/li (h/span {:class "paper-font-body1"} item))
;;           (h/li :#special (h/span {:class "paper-font-body2"} item))
;;           (h/li (h/span {:class "paper-font-body3"} item)))
;;         )))

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
