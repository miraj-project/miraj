(ns co-ns.config
  (:require [miraj :refer [config >> >>!]]
            [polymer.iron :as iron]
            [miraj.http.response :refer [not-found]]))

(println "LOADING config")

(defn init []
  (println "RUNNING config/init"))

(miraj/config :sync)

(require '[co-ns.meta-test] :reload)

(iron/list "foo")


;; (def dispatch-table {:$ 'hello.world/main
;;                      :employee 'hello.world/employee
;;                      :foo 'hello.world/f1
;;                      :foo.bar 'hello.world/f2
;;                      :* #(not-found (str "not found: " (:uri %)))})


;; >> : OBSERVE THEN EXHIBIT

;; GET:  >>
;; (>> $ hello.world/main)
;; (>> employee hello.world/employee)
;; (>> foo.bar hello.world/f1)
;; (>> foo.bar.baz hello.world/f2)
;; (>> dump hello.world/dump)
;; (>> * #(not-found (str "not found: " (:uri %))))

;; ;; multi-arity fns
;; ;; (>> multi hello.world/multiarity)

;; ;; to get entire rqst as arg, use a lambda wrapper:
;; (>> foo #(hello.world/myfn %)) ;; myfn will be passed rqst as single arg

;; ;; directly deliver EDN literal data!!
;; (>> vec.nbr [1 2 3])
;; (>> vec.sym [a b c])
;; (>> list.nbr (1 2 3))
;; (>> list.sym (a b c))
;; (>> list.quote '(a b c))
;; (>> list.list (list 'a 'b 'c))
;; (>> map {:a 1})
;; (>> nbr 1)
;; (>> str "hello world")
;; (>> char \x)

;; ;; deliver def data!!
;; (>> map.def hello.world/mymap) ;; a def

;; ;; HEAD:  >>?
;; ;; (>>? $ hello.world/main)

;; ;; POST:  >>!  (definitely mutational)
;; (>>! foo hello.world/f1!)
;; (>>! foo.bar hello.world/f2!)

;; ;; PUT:  >>!?  (maybe mutational)
;; ;; (>>!? foo hello.world/f!?)


;; ;; PUSH

;; ;;  <<    (EXHIBIT THEN OBSERVE)
;; ;; (<< foo hello.world/or)

