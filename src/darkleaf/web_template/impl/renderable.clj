(ns darkleaf.web-template.impl.renderable
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  clojure.lang.Var
  (render [this w ctx]
    (p/render @this w ctx))

  clojure.lang.Fn
  (render [this w ctx]
    (p/render (this ctx) w ctx))

  Object
  (render [this writer _]
    (w/append writer (str this))))
