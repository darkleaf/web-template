(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   darkleaf.web-template.impl.attribute-value
   darkleaf.web-template.impl.element
   darkleaf.web-template.impl.renderable
   darkleaf.web-template.impl.value
   [darkleaf.web-template.internal.backtick :refer [template-fn]]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defmacro compile [form]
  `(p/compile-element ~(template-fn form)))

(defn render-to-string [template data]
  (w/write-to-string
   #(p/render template % data)))
