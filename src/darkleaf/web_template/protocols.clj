(ns darkleaf.web-template.protocols)

(defprotocol Renderable
  (render [this writer ctx]))

(defprotocol Element
  (element->renderable [this mode]))

(defprotocol Container
  (container->renderable [this block inverted-block]))

(defprotocol AttributeValue
  (update-attribute-value [patch ctx value])
  #_(attribute-value [this ctx]))
