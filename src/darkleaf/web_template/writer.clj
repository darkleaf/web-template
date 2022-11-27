(ns darkleaf.web-template.writer
  (:import
    (java.io Writer StringWriter)))

(set! *warn-on-reflection* true)

;; TODO: for cljs use https://google.github.io/closure-library/api/goog.string.StringBuffer.html

(defn append-raw [^Writer w ^String str]
  (.append w str))

(defn append [^Writer w ^String str]
  (.append w str))

(defn write-to-string [callback]
  (let [sw (StringWriter.)]
    (callback sw)
    (.toString sw)))
