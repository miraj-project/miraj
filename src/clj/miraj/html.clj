(ns miraj.html
  (:refer-clojure :exclude [map])
  (:require [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [jsoup.soup :as jsoup])
  (:import [java.io ByteArrayInputStream StringReader StringWriter]
           [java.util Properties]
           [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform OutputKeys TransformerFactory]
           [javax.xml.transform.stream StreamSource StreamResult]
           [org.jsoup Jsoup]))

;; ;; using jsoup:
;; (defn pprint
;;   [html]
;;   (let [doc (Jsoup/parseBodyFragment html)]
;;     (.prettyPrint (.outputSettings doc) true)
;;     (println (.html (.body doc)))))

;; TransformerFactory factory = TransformerFactory.newInstance();
;; try {
;; 	Transformer transformer = factory.newTransformer();
;; 	transformer.setOutputProperty(OutputKeys.INDENT, "yes");
;; 	transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");	DOMSource source = new DOMSource(doc);
;; 	StreamResult result = new StreamResult(outputFile);
;; 	transformer.transform(source, result);
;; } catch (TransformerException ex) {
;; 	//Do something with the exception!
;; }

;; http://stackoverflow.com/questions/1264849/pretty-printing-output-from-javax-xml-transform-transformer-with-only-standard-j
(defn pprint-stream
  [html]
  ;; (println "HTML str: " html)
  (let [factory (TransformerFactory/newInstance)
        transformer (.newTransformer factory)]
    (.setOutputProperty transformer OutputKeys/INDENT "yes")
    (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "4")
    (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes")
    (let [xmlInput (StreamSource. (StringReader. html))
          xmlOutput (StreamResult. (StringWriter.))]
    (.setOutputProperty transformer OutputKeys/DOCTYPE_PUBLIC "bar")
    (.setOutputProperty transformer OutputKeys/DOCTYPE_SYSTEM "foo")

    ;; DocumentType doctype = xmlDoc.getDoctype();
    ;;     if(doctype != null) {
    ;;         transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, doctype.getPublicId());
    ;;         transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, doctype.getSystemId());
    ;;     }

      (.transform transformer xmlInput xmlOutput)
      (println (.toString (.getWriter xmlOutput))))))

;; (defn pprint
;;   [html]
;;   ;; (println "HTML: " html)
;;   (let [factory (TransformerFactory/newInstance)
;;         transformer (.newTransformer factory)
;;         html (if (string? html) html
;;                  ;; (if Element
;;                  (xml/serialize html))]
;;     (.setOutputProperty transformer OutputKeys/INDENT "yes")
;;     (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "4")
;;     (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes")
;;     ;; (.setOutputProperty transformer OutputKeys/DOCTYPE_PUBLIC "")
;;     ;; (.setOutputProperty transformer OutputKeys/DOCTYPE_SYSTEM "foo")
;;     ;; (.setOutputProperty transformer OutputKeys/METHOD "html")
;;     (let [dom-factory (DocumentBuilderFactory/newInstance)
;;           builder (.newDocumentBuilder dom-factory)
;;           is (ByteArrayInputStream. (.getBytes html))
;;           doc (.parse builder is)
;;           domsrc (DOMSource. doc)
;;           xmlOutput (StreamResult. (StringWriter.))
;;           os (.getWriter xmlOutput)
;;           doctype (.getDoctype doc)]
;;       (if doctype
;;           (.write os "<!DOCTYPE html>\n"))
;;       (.transform transformer domsrc xmlOutput)
;;       (println (.toString (.getWriter xmlOutput))))))

;; ;; using data.xml:
;; (defn pprint
;;   [html]
;;   (log/trace "html type: " (type html))
;;   (pp/pprint html))

;; from https://github.com/brennonyork/clj-template
;; see http://www.paradiso.cc/2014/03/25/html-templates-in-clojure/

;; see also https://github.com/hoplon/hoplon


(def html5-tags
  ["a" "abbr" "address" "area" "article" "aside" "audio"
   "b" "base" "bdi" "bdo" "blockquote" "body" "br" "button"
   "canvas" "caption" "cite" "code" "col" "colgroup" "command"
   "datalist" "dd" "del" "details" "dfn" "div" "dl" "dt" "data"
   "em" "embed"
   "fieldset" "figcaption" "figure" "footer" "font" "form"
   "h1" "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
   "i" "iframe" "img" "input" "ins"
   "kbd" "keygen"
   "label" "legend" "li" "link"
   "map" "mark" "menu" "menuitem" "meta" "meter" "main" "math"
   "nav" "noscript"
   "object" "ol" "optgroup" "option" "output"
   "p" "param" "pre" "progress"
   "q"
   "rp" "rt" "ruby"
   "s" "samp" "script" "section" "select" "small" "source" "span" "strong" "style" "sub" "summary" "sup" "svg"
   "table" "tbody" "td" "textarea" "tfoot" "th" "thead" "time" "title" "tr" "track"
   "u" "ul"
   "var" "video"
   "wbr"])

(def polymer-tags
  ["dom-module" "dom-repeat" "template"])

(defn make-attrs
  [attrs]
  (string/join " "
    (for [[k v] attrs]
      (str (name k) "=\""v "\""))))

(defmacro xform-args
  [forms]
  (log/trace "xform-args: " forms (type forms))
  (loop [lst# forms rst# '()]
    (log/trace "loop lst#: " lst# " rst#: " rst#)
    (cond
      (empty? lst#)
      (do (log/trace "empty?: " (reverse rst#) (type (reverse rst#)))
          (list 'quote (reverse rst#)))

      (map? (first lst#))
      (recur (next lst#) rst#)

      (string? (first lst#))
      (do (log/trace "string: " (first lst#))
          (recur (next lst#) (cons (first lst#) rst#)))

      (symbol? (first lst#))
          (let [fst (first lst#)]
            (if (contains? (ns-map *ns*) fst)
              (recur (next lst#) (cons (eval fst) rst#))
              (recur (next lst#) (cons (str "{{" fst "}}") rst#))))

      (= (first lst#) 'get-in)
      (do (log/trace "get-in: ")
          (cons "GOTIN" rst#))

      (list? (first lst#))
      (do (log/trace "list? first")
          (let [form (first lst#)
                log (log/trace "form: " form)
;;                log (log/trace "eval form: " ~form)
                op (first form)
                rst (rest form)
                ;; log (log/trace "OP: " op)
                ;; log (log/trace "RST: " rst)
                ]
          (recur (next lst#) (cons (resolve op)
                                   rst#))))
          ;; (recur (next lst#) (cons (eval
          ;;                           (first lst#)) rst#)))
          ;; (recur (next lst#) (cons (xform-args `(first ~lst#)) rst#)))

      :else
      (do (log/trace "else: ")
          (let [fst (first lst#)]
            (if (contains? (ns-map *ns*) fst)
              (recur (next lst#) (cons (eval fst) rst#))
              (recur (next lst#) (cons (str "{{" fst "}}") rst#))))))))

(defmacro eval-body
  [body]
  (log/trace "eval-body: " (str body))
  (if (symbol? body)
    (if (contains? (ns-map *ns*) body)
      body
      (str "{{" body "}}"))
    (apply eval body)))

;; (defmacro elt
;;   [tag & args]
;;   (log/trace "elt tag: " tag)
;;   (log/trace "elt args: " args (count args) (type args))
;; ;;  args
;;   "foo"
;;   )
  ;; (str
  ;;  (str "<" (name tag)
  ;;          ;; (when (clojure.core/map? fst#) ;;args#))
  ;;          ;;   (do (log/trace "map? true")
  ;;          ;;       (str " " (make-attrs fst#))))
  ;;          ">")
  ;;  (str "</" (name tag) ">\n")))

  ;; `(let [;;args# ~@args ;; (xform-args ~args)
  ;;        ;;log# (log/trace "XFORM results: " args# (type args#))
  ;;        ;; fst# (first '~args)
  ;;        res# (list
  ;;                   (str "<" (name ~tag)
  ;;                        ;; (when (clojure.core/map? fst#) ;;args#))
  ;;                        ;;   (do (log/trace "map? true")
  ;;                        ;;       (str " " (make-attrs fst#))))
  ;;                        ">")

  ;;                   (if (clojure.core/map? (first args))
  ;;                     (apply str (rest args))
  ;;                     ~bod)
  ;;                     ;;(eval-body args))
  ;;                             ;; (apply str (rest (quote args#)))
  ;;                             ;; (apply str (quote args#)))

  ;;                   (str "</" (name ~tag) ">\n"))
  ;;        res-str# (apply str res#)]
  ;;    (log/trace "elt res#: " res# (type res#) (count res#))
  ;;    (log/trace "elt res-str#: " res-str#)
  ;;    res-str#)))
;;     (apply str res#)))

;; (defn make-macros
;;   [args]
;;   (log/trace "make-macros " args (type args))
;;   (let [r (doseq [arg args]
;;             (do
;;               (log/trace "makemacs arg: " arg (type arg))
;;               (eval `(defmacro ~(symbol (str arg))
;;                  [& x#]
;;                  (log/trace "FOBARL:" x# (type x#))
;;                  (log/trace "fst:" (first x#))
;;                  (str
;;                   "<"
;;                   ~arg
;;                   (if (map? (first x#)) (str " " (make-attrs (first x#))))
;;                   ">"
;;                   (let [forms# (if (map? (first x#))
;;                                 (rest x#)
;;                                 x#)]
;;                     (log/trace "forms: " forms# (type forms#))
;;                       ;; (cond
;;                       ;;   (symbol? (first forms#))
;;                       ;;   (do (log/trace "FOO")
;;                       ;;       (if (contains? (ns-map *ns*) (first forms#))
;;                       ;;         (apply eval forms#)
;;                       ;;         (str "{{" (apply str forms#) "}}")))
;;                       ;;   (not (nil? forms#))
;;                       ;;   (do (log/trace "BAR")
;;                       ;;       (apply eval forms#))))
;;                     (cond
;;                       (symbol? (first forms#))
;;                       (do (log/trace "FOO")
;;                           (if (contains? (ns-map *ns*) (first forms#))
;;                             (apply eval forms#)
;;                             (str "{{" (apply str forms#) "}}")))
;;                       (not (nil? forms#))
;;                       (do (log/trace "BAR")
;;                           (apply str (for [form# forms#]
;;                                        (do (log/trace "FORM: " form#)
;;                                            (eval form#)))))))
;;                   "</" ~arg ">")))))]
;;     nil))

;; (make-macros html5-tags)

(defn make-fns
  [args]
  (log/trace "make-fns " args) ;; (type args))
  (doseq [arg args]
    (log/trace "make-fns arg: " arg (type arg))
    (let [farg (symbol arg)
          kw   (keyword arg)
          func `(defn ~farg ;; (symbol (str arg))
                  [& hargs#]
                  (log/trace "hargs: " (pr-str hargs#))
                  (if (empty? hargs#)
                    (xml/element ~kw)
                    (let [first# (first hargs#)
                          attrs# (if (map? first#)
                                   (do (log/trace "map? first")
                                       (if (instance? clojure.data.xml.Element first#)
                                         (do (log/trace "Element instance")
                                             {})
                                         (do (log/trace "NOT Element instance")
                                             first#)))
                                   (do (log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? clojure.data.xml.Element first#)
                                       hargs#
                                       (rest hargs#))
                                     hargs#)
                          func# (apply xml/element ~kw attrs# content#)]
                      ;; (log/trace "hargs: " hargs#)
                      (log/trace "kw: " ~kw)
                      (log/trace "args: " attrs#)
                      (log/trace "content: " content# " (" (type content#) ")")
                      ;; (log/trace "func: " func# (type func#))
                      func#)))
          f (eval func)])))

(make-fns html5-tags)
(make-fns polymer-tags)

;; (defn make-str-fns
;;   [args]
;;   (log/trace "make-fns " args (type args))
;;   (let [r (doseq [arg args]
;;             (log/trace "make-fns arg: " arg (type arg))
;;             (let [farg (symbol arg)
;;                   func (eval `(defn ~farg ;; (symbol (str arg))
;;                          [& hargs#]
;;                          (log/trace "FOBARL:" hargs# (type hargs#))
;;                          (log/trace "fst:" (first hargs#))
;;                          (str
;;                           "<"
;;                           ~arg
;;                           (if (map? (first hargs#)) (str " " (make-attrs (first hargs#))))
;;                           ">"
;;                           (apply str
;;                                  (let [forms# (if (map? (first hargs#))
;;                                                 (rest hargs#)
;;                                                 hargs#)]
;;                                    (log/trace "forms#: " forms# (type forms#))
;;                                    (cond
;;                                      (symbol? (first forms#))
;;                                      (do (log/trace "FOO")
;;                                          (if (contains? (ns-map *ns*) (first forms#))
;;                                            (apply eval forms#)
;;                                            (str "{{" (apply str forms#) "}}")))
;;                                      (not (nil? forms#))
;;                                      (do (log/trace "BAR")
;;                                          (apply str (for [form# forms#]
;;                                                       (do (log/trace "FORM: " form#)
;;                                                           (eval form#))))))))
;;                           "</" ~arg ">")))]
;;               (log/trace "func: " func)))]
;;         nil))

;; (make-str-fns html5-tags)
;; (make-str-fns polymer-tags)
;; *ns*
;; (li "hello")

;;(div {:class$ "foo"} "bar")

;;(makemacs polymer-tags)
;; (dom-module {:id "my-elt"} (template "foo"))
