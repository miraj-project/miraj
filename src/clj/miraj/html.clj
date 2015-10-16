(ns miraj.html
  (:require [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.string :as string]
            [clojure.pprint :as pp]))

(defn unroll
  [args]
  (log/trace "unrolling " args (type args))
  (let [result (cond
                 (string? args)
                 args

                 (seq? args)
                 (do (log/trace "nbr args: " (count args))
                     (string/join "" (for [arg args] (str arg))))
                                    ;; (do
                                    ;;   (log/trace "ARG " arg (type arg))
                                    ;;   (if (string? arg)
                                    ;;     arg
                                    ;;     (string/join \newline (str arg)))))))
                 :else (throw (RuntimeException. (str "unexpected arg type: " (type args)))))]
    result))

(defn- make-attrs
  [attrs]
  (string/join " "
    (for [[k v] attrs]
      (str (name k) "=\""v "\""))))

(defn link
  [attrs]
  (str "<link " (make-attrs attrs) ">" \newline))

(defn style
  [& args]
  (str "<style"
    (if (map? (first args))
      (let [unrolled (unroll (rest args))]
        (log/trace "unrolled: " unrolled)
        (str " " (make-attrs (first args)) ">" unrolled))
      (str ">" (unroll (first args))))
    "</style>"))

;; INLINE ELEMENTS
(defn span
  [& args]
  (log/trace "span: " args (count args))
  (str "<span"
    (if (map? (first args))
      (let [unrolled (unroll (rest args))] ; <- apply forces eval of lazy seq!
        (log/trace "unrolled: " unrolled)
        (str " " (make-attrs (first args)) ">" unrolled))
      (str ">" (unroll (first args))))
    "</span>"))

(defn i
  [& args]
  ;; (println "italics " args)
  (if (string? args)
    (str "<i>" args "</i>")
    (str "<i"
      (if (map? (first args))
        (str " " (make-attrs (first args)) ">" (unroll (rest args)))
        (str ">" (unroll (first args))))
      "</i>")))

;; BLOCK ELEMENTS
(defn ol
  [& args]
  (str "<ol"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
      (str ">" (unroll (first args))))
    "</ol>"))

(defn ul
  [& args]
  (str \newline "<ul"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
      (str ">" (unroll args)))
    \newline "</ul>"))

(defn li
  [& args]
  (log/trace "li " args)
  (str "\n<li"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
      (str ">" (apply unroll args)))
    "</li>"))

(defn dom-module
  [& args]
  (str "<dom-module"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (second args)))
      (str ">" (unroll args)))
    \newline "</dom-module>"))

(defn template
  [& args]
  (str \newline "<template"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
      (str ">" \newline (unroll args)))
    \newline "</template>"))
