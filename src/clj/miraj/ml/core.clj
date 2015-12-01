(ns miraj.ml.core
  (:refer-clojure :exclude [map meta time])
  (:require [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]))


  ;; (:import ;; [java.io ByteArrayInputStream StringReader StringWriter]
           ;; [java.util Properties]
           ;; [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           ;; [javax.xml.transform.dom DOMSource]
           ;; [javax.xml.transform OutputKeys TransformerFactory]
           ;; [javax.xml.transform.stream StreamSource StreamResult]
           ;; [org.jsoup Jsoup]
           ;; [org.jsoup.parser Parser])

(defn make-fns
  [args]
  ;; (log/trace "make-fns " args) ;; (type args))
  (doseq [arg args]
    ;; (log/trace "make-fns arg: " arg (type arg))
    (let [farg (symbol arg)
          kw   (keyword arg)
          func `(defn ~farg ;; (symbol (str arg))
                  [& hargs#]
                  ;; (log/trace "HTML FN: " ~kw (pr-str hargs#))
                  (if (empty? hargs#)
                    (xml/element ~kw)
                    (let [first# (first hargs#)
                          attrs# (if (map? first#)
                                   (do ;(log/trace "map? first")
                                       (if (instance? clojure.data.xml.Element first#)
                                         (do ;(log/trace "Element instance")
                                             {})
                                         (do ;(log/trace "NOT Element instance")
                                             first#)))
                                   (do ;(log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? clojure.data.xml.Element first#)
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

(defn make-void-elt-fns
  [args]
  ;; (log/trace "make-void-elt-fns " args) ;; (type args))
  (doseq [arg args]
    ;; (log/trace "make-void-elt-fns fn: " arg) ;; (type arg))
    (let [farg (symbol arg)
          kw   (keyword arg)
          func `(defn ~farg ;; (symbol (str arg))
                  [& hargs#]
                  ;; (log/trace "HTML VOID FN: " ~kw (pr-str hargs#))
                  (if (empty? hargs#)
                    (xml/element ~kw)
                    (if (not (map? (first hargs#)))
                      (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                      (if (instance? clojure.data.xml.Element (first hargs#))
                        (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                        (if (not (empty? (rest hargs#)))
                          (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                        (let [func# (apply xml/element ~kw hargs#)]
                        ;;   ;; (log/trace "hargs: " hargs#)
                          ;; (log/trace "kw: " ~kw)
                          ;; (log/trace "args: " (first hargs#))
                          ;; (log/trace "func: " func# (type func#))
                          func#))))))
          f (eval func)])))
