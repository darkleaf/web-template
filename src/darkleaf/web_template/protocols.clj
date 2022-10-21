(ns darkleaf.web-template.protocols
 (:import
  (java.io Writer)))

(defprotocol Component
  (render
    [this ^Writer writer ctx attrs]
    [this ^Writer writer ctx attrs block inverted-block]))
