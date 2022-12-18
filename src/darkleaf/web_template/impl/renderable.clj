(ns darkleaf.web-template.impl.renderable
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defn render-seqable [this w ctx]
  (doseq [item this]
    (p/render item w ctx)))

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
      (seqable? this) (render-seqable this w ctx)
      :default        (render-default this w ctx))))
