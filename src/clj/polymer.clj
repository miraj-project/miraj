(ns polymer
  (:refer-clojure :exclude [list map meta time])
  (:require [miraj.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [cheshire.core :as json :refer :all]))

;;(ns-unmap *ns* 'make-polymer-fns)

(defn make-polymer-fns
  [pfx args]
  (log/trace "make-polymer-fns " pfx) ;; " " args) ;; (type args))
  (doseq [arg args]
    (let [farg (symbol arg)
          kw (keyword (str pfx "-" arg))
          ;; log (println "make-polymer-fns arg: " farg " (" arg ")")
          func `(defn ~farg ;; (symbol (str arg))
                  [& hargs#]
                  ;; (println "POLYMER FN: " ~kw (pr-str hargs#))
                  (if (empty? hargs#)
                    (xml/element ~kw)
                    (let [first# (first hargs#)
                          attrs# (if (map? first#)
                                   (do ;(log/trace "map? first")
                                       (if (instance? miraj.data.xml.Element first#)
                                         (do ;(log/trace "Element instance")
                                             {})
                                         (do ;(log/trace "NOT Element instance")
                                             first#)))
                                   (do ;(log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? miraj.data.xml.Element first#)
                                       hargs#
                                       (rest hargs#))
                                     hargs#)
                          func# (apply xml/element ~kw attrs# content#)]
                      ;; (log/trace "hargs: " hargs#)
                      ;; (log/trace "kw: " ~kw)
                      ;; (log/trace "args: " attrs#)
                      ;; (log/trace "content: " content# " (" (type content#) ")")
                      ;; (log/trace "func: " func# (type func#))
                      func#)))
          f (eval func)])))





