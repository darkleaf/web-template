(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   darkleaf.web-template.impl.element
   darkleaf.web-template.impl.renderable
   darkleaf.web-template.impl.value

   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]
   [darkleaf.web-template.internal.backtick :refer [template-fn]]))

(set! *warn-on-reflection* true)

(defmacro compile [form]
  `(p/compile-element ~(template-fn form)))

(defn render-to-string
  ([template data]
   (render-to-string template nil data))
  ([template ctx data]
   (w/write-to-string
    #(p/render template % (p/ctx-push ctx data)))))
