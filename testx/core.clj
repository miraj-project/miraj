(println (ana.jvm/analyze '(defn f [])))

;; (e/emit-hygienic-form (ana.jvm/analyze '(defn f [])))

;; (ast/children (ana.jvm/analyze '(let [foo (fn [_])] (foo "bar"))))

(def items ["a" "b" "c"])

(for [item items] (identity item))

(let [e (ana.jvm/analyze '(def items ["a" "b" "c"]))
      env {:locals {'items e}
           :context :ctx/expr
;;           :ns 'miraj.core
           }]
;  (pp/pprint env)
(let [items ["a" "b" "c"]]
  (set! *print-length* 100)
  (set! *print-level* 13)
  (for [item ["a" "b"]] (identity item)))



;;(meta (var "hello"))

(let [f (ana.jvm/analyze '(let [items ["a" "b"]] (for [item items] (identity item))))]

  (pp/pprint f))
  (pp/pprint (e/emit-hygienic-form f)))

;; ))

;;(pp/pprint (ana.jvm/analyze '(for [item items")))

(pp/pprint (ana.jvm/analyze '(do 1 2 :foo)))

(e/emit-form (ana.jvm/analyze '(let [a 1] a)))

(pp/pprint (ast/children (ana.jvm/analyze '[1 (+ 1 2)])))

(pp/pprint (ast/nodes (ana.jvm/analyze '[1 (+ 1 2)])))

