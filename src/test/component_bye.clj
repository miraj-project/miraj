(ns component-bye
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [miraj.core :as miraj]
            [miraj.compiler :as wc]
            [miraj.co-dom :as x]
            [miraj.html :as h]
            [miraj.polymer.Protocols :as protocols]
            [miraj.polymer.paper :as paper]
            ;; [miraj.polymer.paper.behavior :as behaviors]
            [miraj.polymer.Events :as events]
            [miraj.polymer.dom :as dom]
            :reload))

;;(println "loading org.example.greetings.arrival")

;(ns-unalias *ns* 'paper)

(miraj/defweb-codom hello-codom
  "hello-codom docstring"
  [greeting]
  ;; (:require [miraj.polymer.paper :as paper :refer [button]])
  ;; (:style  '(styles.shared shared-styles))
  (:codom
   (h/style ":host {display: block;} span {background-color:teal;}")
   (h/h2 ::special.page-title :greeting) ;; (x/element :content))
   (h/span ::.paper-font-body2 "Update text to change the greeting.")
   ;; Listens for "input" event and sets greeting to <input>.value
   (h/input {:class "paper-font-body2" :value :input->greeting})
   #_(paper/button "hi")))

;; (pp/pprint hello-codom)

;; (print (x/serialize hello-codom))

;; (x/pprint hello-codom)

(miraj/defweb-properties AProps
  ;; hostAttributes map
  {:string-attribute "Value"
   :boolean-attribute true
   :tabindex 0}
  (^Boolean president true :read-only)
  (^Number x 0 (fn [new old] (+ new old)) :notify :reflect)
  (^String lname "Lincoln" (fn [new old] (.log js/console
                                               (str "Old pres: " old "; new: " new)))))
;; AProps
;; (pp/pprint AProps)

(miraj/defweb-properties BProps
  "BProps docstring here"
  (^Boolean bool-b true :read-only)
  (^Number ^{:doc "number y docstring"} y 99)
  (^String greeting "Howdy Miraj!" (fn [new old] (.log js/console (str "MSG CHANGE: " new)))))



(miraj/defcomponent [bye greetings-goodbye] ;; [fn html-tag]
  "components.greetings/hello custom component"
  hello-codom

  ;;Properties
  AProps
  BProps

  ;;Protocol Implementations
  protocols/This
  ;; private
  (_foo [] (.log js/console "FOO"))
  ;; public
  (bar [] (.log js/console "BAR"))

  protocols/Lifecycle
  (created [] (.log js/console "CREATED"))
  (attached [] (.log js/console "ATTACHED"))

  ;; FIXEM: protocols, not events. an event suite is a protocol

  events/Gesture
  (with-element :special (tap [e] (.log js/console "you tapped the h1 element")))
  (down [x] (do (.log js/console "DOWN")))

  events/Mouse
  (click [x] (.log js/console "CLICK"))
  (dblclick [x] (.log js/console "DBLCLICK"))
  (mouseover [x] (.log js/console "MOUSEOVER"))

  ;; FIXME:  extensions, not behaviors or mixins
  ;; mixins
;  behavior/PaperButton
;  paper.extension/button
  ;; Polymer.Behaviors.PaperCheckedElement
    )
