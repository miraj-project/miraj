;; we're loaded by config  (println "world.clj ns: " *ns*)
(miraj/co-ns hello.world
       "docstring here"
       (:title "my webpage")
       (:main 'main)
       (:polymer [polymer.paper :as paper :only [button card checkbox dialog dialog-scrollable
                                                 input textarea]]
                 [polymer.iron :as iron :only [flex-layout form]] ;; :refer :all :exclude [behaviors input]]
                 [polymer.neon :as neon :only [animation]])
                 ;; [polymer.font :only [roboto]])
       (:styles [styles.hello.world :css :refer [core]]
                [styles.foo.bar :css :refer [baz]])
       (:scripts [scripts.hello.world :js :refer [core]]
                 [scripts.foo.bar :js :refer [baz]])
                 ;;FIXME: also support: :html, maybe :theme?
       (:require [miraj :as miraj :refer :all] ;;FIXME
                 [miraj.html :as h]
                 [miraj.http.response :refer [ok created]]
                 [cheshire.core :as json :refer :all]
                 ;; [clojure.data.xml :as xml]
                 [clojure.tools.logging :as log :only [trace debug error info]]
                 [clojure.pprint :as pp]
                 [clojure.string :as string]))

(log/trace "loading")

(defn mydialog []
  (paper/dialog {:id "scrolling"}
                (h/h2 "Dialog Title")

                (h/div {:class "horizontal-section"}
                       (h/form {:is "iron-form" :id "formPost" :method "post" :action "/"}
                               (paper/input {:name "applicant-name"
                                             :label "Applicant Name"
                                             :required "required"})
                               (h/br)
                               (paper/input {:name "affiliation"
                                             :label "Affiliation"
                                             :required "required"})

                               (paper/input {:name "address1"
                                             :label "address line 1"
                                             :required "required"})

                               (paper/input {:name "address2"
                                             :label "address line 2"
                                             :required "required"})
                               (paper/input {:name "city"
                                             :label "city"
                                             :required "required"})
                               (paper/input {:name "state"
                                             :label "state"
                                             :required "required"})
                               (paper/input {:name "zip"
                                             :label "zip"
                                             :required "required"})
                               (paper/input {:name "email"
                                             :label "email"
                                             :required "required"})

                               (h/label "Interest")
                               (h/select {:name "interest"}
                                         (h/option {:value "culture"} "Organizational culture")
                                         (h/option {:value "methods"} "New methods")
                                         (h/option {:value "i18n"} "International")
                                         (h/option {:value "synthetic"} "Synthetic data methods")
                                         (h/option {:value "reporting"} "Reporting and storing")
                                         (h/option {:value "practical"} "Practical/Ethical considerations"))

                               (h/br)(h/br)(h/br)
                               (h/div {:class "buttons"}
                                       ;; (paper/button "More Info...")
                                       (paper/button {:dialog-dismiss ""} "Cancel")
                                       ;; (paper/button {:dialog-confirm "" :autofocus ""} "Accept")
                                       (paper/button {:dialog-confirm "dialog-confirm"
                                                      :onclick "submitHandler(event)"
                                                      :autofocus "autofocus"} "Submit"))))))
                                                      ;; :raised "raised"

(co-fn main []
       (log/info "loading hello.world/main")
       (h/body
        (h/div {:id "cards"}
               (paper/card
                (h/div {:class "card-content"}
                       (h/div "Hello, world!"
                              (h/br)(h/br)
                              (h/span "Hoorah!")))))

        (paper/textarea)

        (h/div
         (paper/button {:data-dialog "scrolling"
                        :raised "raised"
                        :onclick "clickHandler(event)"} "Register"))
         (mydialog)
         (h/br)(h/br)
         (h/div {:class "horizontal-section-container"}
                (h/div
                 (h/h4 "Submitted form data")
                 (h/div {:class "horizontal-section wide"}
                        (h/div {:id "output"}))))

         (h/script "
    document.getElementById('formPost').addEventListener('iron-form-submit', display);
    function display(event) {
      var output = document.getElementById('output');
      output.innerHTML = JSON.stringify(event.detail);
    }
    function submitHandler(event) {
      Polymer.dom(event).localTarget.parentElement.parentElement.submit();
    }
    function resetHandler(event) {
      Polymer.dom(event).localTarget.parentElement.reset();
    }")

        (h/script  "
    function clickHandler(e) {
      var button = e.target;
      if (!button.hasAttribute('data-dialog')) {
        return;
      }
      var id = button.getAttribute('data-dialog');
      var dialog = document.getElementById(id);
      if (dialog) {
        dialog.open();
      }
    }")))

;; params
;;   ^:?  a = a is a required query/body param
;;   ^:?? a = a is an optional query/body param
;;   ^:meta uri = uri is a rqst meta-param, e.g. :uri
(defn employee [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "employee " lname status fname)
  (ok (json/generate-string {:lname lname :fname fname :status status :foo foo})))


(defn f1 [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "f1 " lname status fname)
  (ok (json/generate-string {:fn :f1 :lname lname :fname fname :status status :foo foo})))

(defn f2 [lname fname mi ^:? status ^:?? foo] ; & remainder]
  (log/trace "f2 " lname status fname)
  (ok (json/generate-string {:fn :f2 :lname lname :fname fname :mi mi
                             :status status :foo foo})))

(defn myfn [rqst]
  (log/trace "myfn " rqst)
  (ok (json/generate-string {:fn :myfn :uri (:uri rqst)})))

(defn multiarity
  ([a]
   (log/trace "multiarity 1:" a)
   (ok (json/generate-string {:fn "multiarity 1" :a a})))
  ([a b]
   (log/trace "multiarity 2:" a b)
   (ok (json/generate-string {:fn "multiarity 2" :a a :b b}))))

(def mymap {:a "hello" :b "world"})

(defn f1! [^:? lname ^:? fname]
  (log/trace "f2 " lname fname)
  (created (json/generate-string {:fn :f1! :lname lname :fname fname})))

(defn f2! [foo ^:? lname ^:? fname]
  (log/trace "f2 " lname fname)
  (created (json/generate-string {:fn :f2! :foo foo :lname lname :fname fname})))

;; accepted 202, non-authoritative-information 203, no-content 204

(defn dump
  []
  (ok (miraj/dump-dispatch-map)))
