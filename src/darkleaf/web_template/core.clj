(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   darkleaf.web-template.impl.attribute-value
   darkleaf.web-template.impl.container
   darkleaf.web-template.impl.element
   darkleaf.web-template.impl.renderable
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(def html5-mode
  {:void-elements    #{"area"
                       "base"
                       "br"
                       "col"
                       "command"
                       "embed"
                       "hr"
                       "img"
                       "input"
                       "keygen"
                       "link"
                       "meta"
                       "param"
                       "source"
                       "track"
                       "wbr"}
   :empty-attributes true})

(def xml-mode
  {:void-elements    #{}
   :empty-attributes false})

(defn compile
  ([form]
   (compile form html5-mode))
  ([form mode]
   (p/element->renderable form mode)))

(defn render-to-string [template data]
  (w/write-to-string
   #(p/render template % data)))
