(ns darkleaf.web-template.protocols
 (:import
  (java.io Writer)))

(set! *warn-on-reflection* true)

(definterface Template
  (^void render [^java.io.Writer writer ctx]))

(defprotocol Component
  (render
    [this ^Writer writer ctx attrs]
    [this ^Writer writer ctx attrs
     ^Template block ^Template inverted-block]))
