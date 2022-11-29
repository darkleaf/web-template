(ns darkleaf.web-template.protocols)

(defprotocol Element
  (compile-element [this opts]))

(defprotocol Renderable
  (render [this writer ctx]))

(defprotocol Value
  (write-value
    [this writer ctx]
    [this writer ctx block inverted-block]))

(defprotocol AttributeValue
  (update-attribute-value [patch ctx value]))
