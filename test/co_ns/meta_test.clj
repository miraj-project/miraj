(require 'miraj)
;(println
; (macroexpand
(miraj/co-ns meta-test
                            "docstring here"
                            (:main index)
                            (:meta :description "Miraj Hello World demo"
                                   :title "Hello World!"
                                   ;; global attrs: http://www.w3.org/html/wg/drafts/html/master/dom.html#global-attributes
                                   ;; meta standard names: :application-name, :author, :description, :generator, :keywords
                                   ;; :charset;  :itemprop?
                                   ;; extensions: https://wiki.whatwg.org/wiki/MetaExtensions
                                   ;; see https://gist.github.com/kevinSuttle/1997924
                                   ;; :viewport gets special treatment
                                   ;; e.g.  :dc {:created ".." :creator "foo" ...}
                                   :application-name "hello-world"
                                   :apple {:mobile-capable :true
                                           :status-bar-style "black"
                                           :title "Hello"
                                           :touch-icon "images/touch/apple-touch-icon.png"}
                                   :msapplication {:navbutton-color "..."
                                                   :tile-color "#3372DF"
                                                   :tile-image "images/ms-touch-icon-144x144-precomposed.png"}
                                   :mobile-capable true
                                   :theme-color "#2E3AA1"))

