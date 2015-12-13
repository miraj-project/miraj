(ns config
  (:require [miraj :refer [>> >>! start]]
            [miraj.http.response :refer [not-found]]
            [ring.util.servlet :as servlet]))

(println "LOADING config")

(defn init []
  (println "RUNNING config/init"))

(miraj/config :sync)

(defn -init
  ([this]
  (println "Servlet Init A called"))
  ([this sc]
   (.initParent this sc)
   (println "Servlet Init B called")))

(servlet/defservice miraj.sync/start)

(println "config DONE, requiring hello.world")

;;(load "/hello/world")
(require '[hello.world])

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

(hello.world/dump)

(servlet/defservice start)
