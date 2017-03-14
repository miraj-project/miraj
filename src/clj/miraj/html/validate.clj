(ns ^{:author "Gregg Reynolds"}
  miraj.html.validate
  (:require [clojure.spec :as spec]
            [miraj.html :as html]))

(defn metas
  [html-tags]
  (spec/check-asserts true)
  (spec/assert ::html/meta html-tags)
  #_(spec/check-asserts false))

