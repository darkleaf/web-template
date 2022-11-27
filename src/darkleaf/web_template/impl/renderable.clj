(ns darkleaf.web-template.impl.renderable
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defn render-fn [this w ctx]
  (p/render (this ctx) w ctx))

(defn render-default [this w ctx]
  (w/append w (str this)))

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  clojure.lang.Var
  (render [this w ctx]
    (p/render @this w ctx))

  Object
  (render [this w ctx]
    (cond
      (fn? this) (render-fn this w ctx)
      :default   (render-default this w ctx))))
