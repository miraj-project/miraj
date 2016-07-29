(ns miraj.polymer.Behaviors)

;; (alter-meta! *ns*
;;              (fn [m] (assoc m :co-ns true
;;                             :resource-type :polymer-behaviors
;;                             :resource-pfx "bower_components")))

(def pfx "/bower_components")

;; (def polymer.behaviors
;;   {:IronA11yKeysBehavior "iron-a11y-keys-behavior/iron-a11y-keys-behavior.html"
;;    :IronButtonState "iron-behaviors/iron-button-state.html"
;;    :IronControlState "iron-behaviors/iron-control-state.html"
;;    :IronCheckedElementBehavior "iron-checked-element-behavior/iron-checked-element-behavior.html"
;;    :IronFitBehavior "iron-fit-behavior/iron-fit-behavior.html"
;;    :IronFormElementBehavior "iron-form-element-behavior/iron-form-element-behavior.html"
;;    :IronMenuBehavior "iron-menu-behavior/iron-menu-behavior.html"
;;    :IronMenubarBehavior "iron-menu-behavior/iron-menubar-behavior.html"
;;    :IronOverlayBackdrop "iron-overlay-behavior/iron-overlay-backdrop.html"
;;    :IronOverlayBehavior "iron-overlay-behavior/iron-overlay-behavior.html"
;;    :IronOverlayManager "iron-overlay-behavior/iron-overlay-manager.html"
;;    :IronRangeBehavior "iron-range-behavior/iron-range-behavior.html"
;;    :IronResizableBehavior "iron-resizable-behavior/iron-resizable-behavior.html"
;;    :IronValidatableBehavior "iron-validatable-behavior/iron-validatable-behavior.html"
;;    :IronValidatorBehavior "iron-validator-behavior/iron-validator-behavior.html"
;;    :PaperButton "paper-behaviors/paper-button-behavior.html"
;;    :PaperCheckedElement "paper-behaviors/paper-checked-element-behavior.html"
;;    :PaperInkyFocusBehavior "paper-behaviors/paper-inky-focus-behavior.html"
;;    :PaperRippleBehavior "paper-behaviors/paper-ripple-behavior.html"
;;    :PaperDialogBehavior "paper-dialog-behavior/paper-dialog-behavior.html"})

(defprotocol ^{:resource-type :polymer-behaviors
               :resource-name "Polymer.PaperButtonBehavior"
               :uri (str pfx "/paper-behaviors/paper-button-behavior.html")
               :properties [:active :noink]
               :listeners true}
  PaperButton)
  ;; #_(active [x])
  ;; #_(method1 [x] [x y])
  ;; #_(noink [x]))

(defprotocol ^{:resource-type :polymer-behaviors
               :resource-name "Polymer.PaperCheckedElementBehavior"
               :uri (str pfx "/paper-behaviors/paper-checked-element-behavior.html")
               :properties [:active :noink]
               :listeners true}
  PaperCheckedElement)
  ;; #_(active [x])
  ;; #_(method1 [x] [x y])
  ;; #_(noink [x]))


(defprotocol ^{:uri (str pfx "/iron-behaviors/iron-button-state.html")} IronButtonState)
