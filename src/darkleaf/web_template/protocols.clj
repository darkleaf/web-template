(ns darkleaf.web-template.protocols)

(defprotocol Component
  (write [this writer ctx attrs block inverted-block]))
