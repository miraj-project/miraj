(ns config
  (:require [hello.world]
            [miraj :refer [>> >>!]]
            [miraj.http.response :refer [not-found]]))

;; (def dispatch-table {:$ 'hello.world/main
;;                      :employee 'hello.world/employee
;;                      :foo 'hello.world/f1
;;                      :foo.bar 'hello.world/f2
;;                      :* #(not-found (str "not found: " (:uri %)))})


;; >> : OBSERVE THEN EXHIBIT

;; GET
(>> $ hello.world/main)
(>> employee hello.world/employee)
(>> foo hello.world/f1)
(>> foo.bar hello.world/f2)
(>> dump hello.world/dump)
(>> * #(not-found (str "not found: " (:uri %))))

;; HEAD
;; (>>? $ hello.world/main)

;; POST - definitely mutational
(>>! foo hello.world/f1!)
(>>! foo.bar hello.world/f2!)

;; PUT - maybe mutational
;; (>>!? foo hello.world/f!?)


;; ;; << : EXHIBIT THEN OBSERVE
;; (<< foo hello.world/or)

