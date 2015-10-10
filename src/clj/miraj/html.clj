(ns miraj.html
  (:require [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.string :as string]
            [clojure.pprint :as pp]))

(defn- unroll
  [args]
;;  (println "unrolling " args)
  (string/join \newline
    (for [arg args]
      (if (string? arg)
        arg
        arg))))

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
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
      (str ">" (unroll (first args))))
    "</style>"))

;; INLINE ELEMENTS
(defn span
  [& args]
  (str "<span"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (unroll (rest args)))
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
  (str "<ul"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" \newline (unroll (rest args)))
      (str ">" \newline (unroll args)))
    \newline "</ul>"))

(defn li
  [& args]
  (str "<li"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" (second args))
      (str ">" (first args)))
    "</li>"))

(defn dom-module
  [& args]
  (str "<dom-module"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" \newline (second args))
      (str ">" args))
    \newline "</dom-module>"))

(defn template
  [& args]
  (str "<template"
    (if (map? (first args))
      (str " " (make-attrs (first args)) ">" \newline (unroll (rest args)))
      (str ">" \newline (unroll args)))
    \newline "</template>"))
