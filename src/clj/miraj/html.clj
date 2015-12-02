(ns miraj.html
  (:refer-clojure :exclude [map meta time])
  (:require [miraj.ml.core :refer [make-fns make-void-elt-fns]]))

            ;;[clojure.data.xml :as xml]
            ;; [clojure.tools.logging :as log :only [trace debug error info]]
            ;; [clojure.string :as string]
            ;; [clojure.pprint :as pp]
  ;; (:import [java.io ByteArrayInputStream StringReader StringWriter]
  ;;          [java.util Properties]
  ;;          [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
  ;;          [javax.xml.transform.dom DOMSource]
  ;;          [javax.xml.transform OutputKeys TransformerFactory]
  ;;          [javax.xml.transform.stream StreamSource StreamResult]
  ;;          [org.jsoup Jsoup]
  ;;          [org.jsoup.parser Parser]))

;; http://www.w3.org/TR/html51/semantics.html#semantics

;; complete list of html5 tags:
;; http://www.w3.org/TR/html-markup/elements.html
;; as of 29 Nov 2015 (missing 'data'):

;; see also http://www.w3.org/html/wg/drafts/html/master/index.html#elements-3

;;FIXME: make a complete tag map, tags as keys, docstrings as vals, linked to w3c spec
;; then use element content categories with membership test
;; a – hyperlink CHANGED
;; abbr – abbreviation
;; address – contact information
;; area – image-map hyperlink
;; article – article NEW
;; aside – tangential content NEW
;; audio – audio stream NEW

;; b – offset text conventionally styled in bold CHANGED
;; base – base URL
;; bdi – BiDi isolate NEW
;; bdo – BiDi override
;; blockquote – block quotation
;; body – document body
;; br – line break
;; button – button

;; canvas – canvas for dynamic graphics NEW
;; caption – table title
;; cite – cited title of a work CHANGED
;; code – code fragment
;; col – table column
;; colgroup – table column group
;; command – command NEW

;; data - MISSING from list but in REC
;; datalist – predefined options for other controls NEW
;; dd – description or value
;; del – deleted text
;; details – control for additional on-demand information NEW
;; dfn – defining instance
;; dialog - dialog box or window; http://www.w3.org/html/wg/drafts/html/master/semantics.html#the-dialog-element
;; div – generic flow container
;; dl – description list
;; dt – term or name

;; em – emphatic stress
;; embed – integration point for plugins NEW

;; fieldset – set of related form controls
;; figcaption – figure caption NEW
;; figure – figure with optional caption NEW
;; footer – footer NEW
;; form – user-submittable form

;; h1 – heading
;; h2 – heading
;; h3 – heading
;; h4 – heading
;; h5 – heading
;; h6 – heading
;; head – document metadata container
;; header – header NEW
;; hr – thematic break CHANGED
;; html – root element

;; i – offset text conventionally styled in italic CHANGED
;; iframe – nested browsing context (inline frame)
;; img – image
;; input – input control CHANGED
;; ins – inserted text

;; kbd – user input
;; keygen – key-pair generator/input control NEW

;; label – caption for a form control
;; legend – title or explanatory caption
;; li – list item
;; link – inter-document relationship metadata

;; main - Container for the dominant contents of another element
;; map – image-map definition
;; mark – marked (highlighted) text NEW
;; math - MathML root
;; menu – menu of commands
;; menuitem - menu item
;; meta – text metadata
;; meter – scalar gauge NEW

;; nav – section with navigational links
;; noscript – fallback content for script

;; object – generic external content: image, nested browsing context, or plugin
;; ol – ordered list
;; optgroup – group of options
;; option – option in a list box or combo box control
;; output – result of a calculation in a form NEW

;; p – paragraph
;; param – initialization parameters for Object
;; pre – preformatted text
;; progress – progress indicator NEW

;; q – quoted text

;; rb - ruby base
;; rp – ruby parenthesis
;; rt – ruby annotation text
;; rtc – ruby annotation text container
;; ruby – ruby annotation

;; s – struck text CHANGED
;; samp – (sample) output
;; script – embedded script
;; section – section NEW
;; select – option-selection form control
;; small – side comment
;; source – media source for video or audio
;; span – generic span
;; strong – strong importance
;; style – style (presentation) information
;; sub – subscript
;; summary – summary, caption, or legend for a details control
;; sup – superscript
;; svg - SVG root
;; table – table
;; tbody – table row group
;; td – table cell
;; template - template
;; textarea – text input area
;; tfoot – table footer row group
;; th – table header cell
;; thead – table heading group
;; time – date and/or time NEW
;; title – document title
;; tr – table row
;; track – timed text track
;; u – unarticulated non-textual annotation
;; ul – unordered list
;; var – variable or placeholder text
;; video – video NEW
;; wbr – line-break opportunity NEW

;; http://www.w3.org/TR/2011/WD-html5-20110525/content-models.html#metadata-content-0
(def html5-metadata-tags
  ["base" "command" "link" "meta" "noscript" "script" "style" "title"
   "template"])

;; http://www.w3.org/TR/2011/WD-html5-20110525/content-models.html#sectioning-content-0
(def html5-sectioning-tags
  ["address" "article" "aside"
   "blockquote" "body" "details"
   "fieldset" "figure" "footer"
   "header" "nav" "section" "td"])

;; http://www.w3.org/TR/2011/WD-html5-20110525/content-models.html#heading-content-0
(def html5-heading-tags
  ["h1" "h2" "h3" "h4" "h5" "h6"])

;; according to https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements
(def html5-block-tags
  ["address"
   "canvas" "caption" "command"
   "datalist" "dd" "del" "details" "dfn" "div" "dl" "dt" "data"
   "figcaption" "form"
   "li"
   "main" "menu" "menuitem"
   "ol"
   "p" "pre"
   "title"
   "ul"])

;; phrasing content: http://www.w3.org/TR/2011/WD-html5-20110525/content-models.html#phrasing-content-0
(def html5-phrasing-tags
  ["a" "abbr" "area" "audio"
   "b" "bdi" "bdo" "br" "button"
   "cite" "code" "command"
   "datalist" "del" "dfn"
   "em" "embed"
   "i" "iframe" "img" "input" "ins"
   "kbd" "keygen"
   "label"
   "map" "mark" "math" "meter"
   "noscript"
   "object" "output"
   "progress"
   "q"
   "ruby"
   "s" "samp" "script" "select" "small" "span" "strong" "sub" "sup" "svg"
   "textarea" "time"
   "u"
   "var" "video"
   "wbr"])

(def html5-null-tags
  ["caption" "col" "colgroup"
   "head" "html"
   "legend"
   "optgroup" "option"
   "rp" "rt"
   "summary"
   "table" "tbody" "td" "tfoot" "th" "thead" "tr"])

(def html5-void-elt-tags
  ["area" "base" "br" "col"
   "embed" "hr" "img" "input"
   "keygen" "link" "meta" "param"
   "source" "track" "wbr"])

(def html5-rawtext-elts
  ["script" "style"])

;; http://www.w3.org/TR/html5/obsolete.html#non-conforming-features
(def html5-obsolete-tags
  {:applet "Use embed or object instead."
   :acronym "Use abbr instead."
   :basefont "Use appropriate elements or CSS instead."
   :bgsound "Use audio instead."
   :big "Use appropriate elements or CSS instead."
   :blink "Use appropriate elements or CSS instead."
   :center "Use appropriate elements or CSS instead."
   :dir "Use ul instead."
   :font "Use appropriate elements or CSS instead."
   :marquee "Use appropriate elements or CSS instead."
   :multicol "Use appropriate elements or CSS instead."
   :nobr "Use appropriate elements or CSS instead."
   :frame "Either use iframe and CSS instead, or use server-side includes to generate complete pages with the various invariant parts merged in."
   :frameset "Either use iframe and CSS instead, or use server-side includes to generate complete pages with the various invariant parts merged in."
   :noframes "Either use iframe and CSS instead, or use server-side includes to generate complete pages with the various invariant parts merged in."
   :hgroup "To mark up subheadings, consider putting the subheading into a p element after the h1- h6 element containing the main heading, or putting the subheading directly within the h1- h6 element containing the main heading, but separated from the main heading by punctuation and/or within, for example, a span class='subheading' element with differentiated styling. Headings and subheadings, alternative titles, or taglines can be grouped using the header or div elements."
   :isindex "Use an explicit form and text field combination instead."
   :listing "Use pre and code instead."
   :nextid "Use GUIDs instead."
   :noembed "Use object instead of embed when fallback is necessary."
   :plaintext "Use the 'text/plain' MIME type instead."
   :strike "Use del instead if the element is marking an edit, otherwise use s instead."
   :xmp "Use pre and code instead, and escape '<' and '&' characters as '&lt;' and '&amp;' respectively."
   :spacer "Use appropriate elements or CSS instead."
   :tt "Use appropriate elements or CSS instead."})

(def attrs-regex
  #" *[^>]* *>")

(def attrs-overlap-start-regex
  ;; e.g. for a, b, em, i, li etc
  ;; match <li>, <li >, <li/>, <li />
  #" */?>")

(def attrs-overlap-attrs-regex
  ;; e.g. for a, b, em, i, li etc
  ;; match <li>, <li >, <li/>, <li />, <li foo="bar">, <li foo="bar">
  #" +[^>]* */?>")

(def html5-tags
  (distinct
   (sort
    (concat html5-null-tags
            html5-metadata-tags html5-heading-tags html5-sectioning-tags html5-block-tags
            html5-phrasing-tags))))

(defn list-tags
  []
  (let [content-tags (remove (set html5-void-elt-tags) html5-tags)]
    (println "ALL TAGS: " html5-tags)
    (println "CONTENT TAGS: " content-tags)
    (doseq [tag html5-tags]
      (println tag))))

(make-fns (remove (set html5-void-elt-tags) html5-tags))

(make-void-elt-fns html5-void-elt-tags)

