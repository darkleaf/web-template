(ns darkleaf.web-template.writer
  (:import
    (java.io Writer)))

;; TODO: for cljs use https://google.github.io/closure-library/api/goog.string.StringBuffer.html

(defn append-raw [^Writer w ^String str]
  (.append w str))

(defn append [^Writer w ^String str]
  (.append w str))
