(ns darkleaf.web-template.context)

(defn push [ctx v]
  (merge (if (map? ctx) ctx)
         (if (map? v) v)
         {'this v}))

(defn this [ctx]
  (if (map? ctx)
    (ctx 'this ctx)
    ctx))
