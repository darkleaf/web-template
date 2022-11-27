(ns darkleaf.web-template.protocols)

(defprotocol Element
  (compile-element [this]))

(defprotocol Renderable
  (render [this writer ctx]))

(defprotocol Value
  (write
    [this writer ctx]
    [this writer ctx block inverted-block]))

(defn ctx-push [ctx v]
  (merge ctx
         (if (map? v) v)
         {'this v}))
