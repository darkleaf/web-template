(ns darkleaf.web-template.protocols
  (:import
   (java.io Writer)))

(set! *warn-on-reflection* true)

(defprotocol Template
  (render-tmpl [this writer ctx]))

(defprotocol Component
  (render
    [this writer ctx attrs]
    [this writer ctx attrs block-tmpl inverted-block-tmpl]))

(defn append [^Writer w ^String str]
  (.append w str))
