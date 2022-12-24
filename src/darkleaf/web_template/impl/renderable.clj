(ns darkleaf.web-template.impl.renderable
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defn render-seqable [this w ctx]
  (doseq [item this]
    (p/render item w ctx)))

(defn render-default [this w ctx]
  (w/append w (str this)))

;; todo: add test
(defn render-map [this w ctx]
  (if-some [template (:template this)]
    (p/render template w (merge ctx this {'this this}))
    (render-default this w ctx)))

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  clojure.lang.Var
  (render [this w ctx]
    (p/render @this w ctx))

  Object
  (render [this w ctx]
    (cond
      (map? this)     (render-map this w ctx)
      (seqable? this) (render-seqable this w ctx)
      :default        (render-default this w ctx))))
