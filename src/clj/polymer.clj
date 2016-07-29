(ns polymer
  (:refer-clojure :exclude [list meta]))
  ;; (:require [miraj.markup :refer [make-resource-fns]]))

(println "loading polymer")

(defprotocol ^{:co-protocol? true}
  This)

(defprotocol ^{:co-protocol? true}
  Lifecycle
  (created [this])
  (attached [this])
  (detached [this])
  (attributeChanged [this])
  (ready [this]))

(defprotocol ^{:co-protocol? true}
  Http
  (request [rqst])
  (response [rsp])
  (error [e]))

(def iron  ;; [kw docstring]
  {:a11y-announcer "iron-a11y-announcer is a singleton element that is intended to add a11y to features that require on-demand announcement from screen readers. In order to make use of the announcer, it is best to request its availability in the announcing element."
   :a11y-keys ""
   :a11y-keys-behavior ""
   :ajax ""
   :autogrow-textarea ""
   :behaviors ""
   :checked-element-behavior ""
   :collapse ""
   :component-page ""
   :doc-viewer ""
   :dropdown ""
   :fit-behavior ""
   :flex-layout ""
   :form ""
   :form-element-behavior ""
   :icon ""
   :icons ""
   :iconset ""
   :iconset-svg ""
   :image ""
   :input ""
   :jsonp-library ""
   :label ""
   :list ""
   :localstorage ""
   :media-query ""
   :menu-behavior ""
   :meta ""
   :overlay-behavior ""
   :pages ""
   :range-behavior ""
   :request ["iron-ajax/iron-request.html" ""]
   :resizable-behavior ""
   :selector ""
   :signals ""
   :swipeable-container ""
   :test-helpers ""
   :validatable-behavior ""
   :validator-behavior ""})

(def paper
  ;; entries:  [kw docstring]
  {:badge ""
   :button "paper-button is a button. When the user touches the button, a ripple effect emanates from the point of contact. It may be flat or raised. A raised button is styled with a shadow."
   :card ""
   :checkbox ""
   :dialog ""
   :dialog-behavior ""
   :dialog-scrollable ""
   :drawer-panel ""
   :dropdown-menu ""
   :fab ""
   :header-panel ""
   :icon-button ""
   :input ""
   :textarea ["paper-input/paper-textarea.html" ""]
   :item ""
   :listbox ""
   :material ""
   :menu ""
   :menu-shared-styles ["paper-menu/paper-menu-shared-styles.html.html" ""]
   :submenu ["paper-menu/paper-submenu.html" ""]
   :menu-button ""
   :menu-button-animations ["paper-menu-button/paper-menu-button-animations.html" ""]
   :progress ""
   :radio-button ""
   :radio-group ""
   :ripple ""
   :scroll-header-panel ""
   :slider ""
   :spinner ""
   :styles ""
   :tab ["paper-tabs/paper-tab.html" "paper-tab is styled to look like a tab. It should be used in conjunction with paper-tabs."]
   :tabs ""
   :toast ""
   :toggle-button ""
   :toolbar ""
   :tooltip ""})


(def iron-old
  {:a11y-announcer [:iron-a11y-announcer "iron-a11y-announcer/iron-a11y-announcer.html"]
   :a11y-keys [:iron-a11y-keys "iron-a11y-keys/iron-a11y-keys.html"]
   :a11y-keys-behavior [:iron-a11y-keys-behavior "iron-a11y-keys-behavior/iron-a11y-keys-behavior.html"]
   :ajax [:iron-ajax "iron-ajax/iron-ajax.html"]
   :autogrow-textarea [:iron-autogrow-textarea "iron-autogrow-textarea/iron-autogrow-textarea.html"]
   :behaviors [:iron-behaviors "iron-behaviors/iron-behaviors.html"]
   :checked-element-behavior [:iron-checked-element-behavior "iron-checked-element-behavior/iron-checked-element-behavior.html"]
   :collapse [:iron-collapse "iron-collapse/iron-collapse.html"]
   :component-page [:iron-component-page "iron-component-page/iron-component-page.html"]
   :doc-viewer [:iron-doc-viewer "iron-doc-viewer/iron-doc-viewer.html"]
   :dropdown [:iron-dropdown "iron-dropdown/iron-dropdown.html"]
   :fit-behavior [:iron-fit-behavior "iron-fit-behavior/iron-fit-behavior.html"]
   :flex-layout [:iron-flex-layout "iron-flex-layout/iron-flex-layout.html"]
   :form [:iron-form "iron-form/iron-form.html"]
   :form-element-behavior [:iron-form-element-behavior "iron-form-element-behavior/iron-form-element-behavior.html"]
   :icon [:iron-icon "iron-icon/iron-icon.html"]
   :icons [:iron-icons "iron-icons/iron-icons.html"]
   :iconset [:iron-iconset "iron-iconset/iron-iconset.html"]
   :iconset-svg [:iron-iconset-svg "iron-iconset-svg/iron-iconset-svg.html"]
   :image [:iron-image "iron-image/iron-image.html"]
   :input [:iron-input "iron-input/iron-input.html"]
   :jsonp-library [:iron-jsonp-library "iron-jsonp-library/iron-jsonp-library.html"]
   :label [:iron-label "iron-label/iron-label.html"]
   :list [:iron-list "iron-list/iron-list.html"]
   :localstorage [:iron-localstorage "iron-localstorage/iron-localstorage.html"]
   :media-query [:iron-media-query "iron-media-query/iron-media-query.html"]
   :menu-behavior [:iron-menu-behavior "iron-menu-behavior/iron-menu-behavior.html"]
   :meta [:iron-meta "iron-meta/iron-meta.html"]
   :overlay-behavior [:iron-overlay-behavior "iron-overlay-behavior/iron-overlay-behavior.html"]
   :pages [:iron-pages "iron-pages/iron-pages.html"]
   :range-behavior [:iron-range-behavior "iron-range-behavior/iron-range-behavior.html"]
   :request [:iron-request "iron-ajax/iron-request.html"]
   :resizable-behavior [:iron-resizable-behavior "iron-resizable-behavior/iron-resizable-behavior.html"]
   :selector [:iron-selector "iron-selector/iron-selector.html"]
   :signals [:iron-signals "iron-signals/iron-signals.html"]
   :swipeable-container [:iron-swipeable-container "iron-swipeable-container/iron-swipeable-container.html"]
   :test-helpers [:iron-test-helpers "iron-test-helpers/iron-test-helpers.html"]
   :validatable-behavior [:iron-validatable-behavior "iron-validatable-behavior/iron-validatable-behavior.html"]
   :validator-behavior [:iron-validator-behavior "iron-validator-behavior/iron-validator-behavior.html"]})

(def paper-old
  ;; entries:  [:fn-tag  [elt-kw elt-uri & docstring]]
  {:badge [:paper-badge "paper-badge/paper-badge.html"]
   :button [:paper-button "paper-button/paper-button.html"]
   :card [:paper-card "paper-card/paper-card.html"]
   :checkbox [:paper-checkbox "paper-checkbox/paper-checkbox.html"]
   :dialog [:paper-dialog "paper-dialog/paper-dialog.html"]
   :dialog-behavior [:paper-dialog-behavior "paper-dialog-behavior/paper-dialog-behavior.html"]
   :dialog-scrollable [:paper-dialog-scrollable "paper-dialog-scrollable/paper-dialog-scrollable.html"]
   :drawer-panel [:paper-drawer-panel "paper-drawer-panel/paper-drawer-panel.html"]
   :dropdown-menu [:paper-dropdown-menu "paper-dropdown-menu/paper-dropdown-menu.html"]
   :fab [:paper-fab "paper-fab/paper-fab.html"]
   :header-panel [:paper-header-panel "paper-header-panel/paper-header-panel.html"]
   :icon-button [:paper-icon-button "paper-icon-button/paper-icon-button.html"]
   :input [:paper-input "paper-input/paper-input.html"]
   :textarea [:paper-textarea "paper-input/paper-textarea.html"]
   :item [:paper-item "paper-item/paper-item.html"]
   :listbox [:paper-listbox "paper-listbox/paper-listbox.html"]
   :material [:paper-material "paper-material/paper-material.html"]
   :menu [:paper-menu "paper-menu/paper-menu.html"]
   :menu-shared-styles [:paper-menu-shared-styles "paper-menu/paper-menu-shared-styles.html.html"]
   :submenu [:paper-submenu "paper-menu/paper-submenu.html"]
   :menu-button [:paper-menu-button "paper-menu-button/paper-menu-button.html"]
   :menu-button-animations [:paper-menu-button-animations "paper-menu-button/paper-menu-button-animations.html"]
   :progress [:paper-progress "paper-progress/paper-progress.html"]
   :radio-button [:paper-radio-button "paper-radio-button/paper-radio-button.html"]
   :radio-group [:paper-radio-group "paper-radio-group/paper-radio-group.html"]
   :ripple [:paper-ripple "paper-ripple/paper-ripple.html"]
   :scroll-header-panel [:paper-scroll-header-panel "paper-scroll-header-panel/paper-scroll-header-panel.html"]
   :slider [:paper-slider "paper-slider/paper-slider.html"]
   :spinner [:paper-spinner "paper-spinner/paper-spinner.html"]
   :styles [:paper-styles "paper-styles/paper-styles.html"]
   :tab [:paper-tab "paper-tabs/paper-tab.html"]
   :tabs [:paper-tabs "paper-tabs/paper-tabs.html"]
   :toast [:paper-toast "paper-toast/paper-toast.html"]
   :toggle-button [:paper-toggle-button	"paper-toggle-button/paper-toggle-button.html"]
   :toolbar [:paper-toolbar "paper-toolbar/paper-toolbar.html"]
   :tooltip [:paper-tooltip "paper-tooltip/paper-tooltip"]})

(println "loaded polymer")
