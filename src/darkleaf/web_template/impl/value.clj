(ns darkleaf.web-template.impl.value
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defn write-value-map
  ([this w ctx]
   (w/append w (str this)))
  ([this w ctx block inverted-block]
   (if (seq this)
     (p/render block w (p/ctx-push ctx this))
     (p/render inverted-block w ctx))))

(defn write-value-seqable
  ([this w ctx]
   (w/append w (str this)))
  ([this w ctx block inverted-block]
   (if (seq this)
     (doseq [item this]
       (p/render block w (p/ctx-push ctx item))
       ;; todo? space
       (w/append-raw w " "))
     (p/render inverted-block w ctx))))

(defn write-value-default
 ([this w _]
  (w/append w (str this)))
 ([this w ctx block inverted-block]
  (p/render block w (p/ctx-push ctx this))))

(extend-protocol p/Value
  nil
  (write-value
    ([this _ _])
    ([this w ctx block inverted-block]
     (p/render inverted-block w ctx)))

  Object
  (write-value
    ([this w ctx]
     (cond
       (map? this)     (write-value-map     this w ctx)
       (seqable? this) (write-value-seqable this w ctx)
       :default        (write-value-default this w ctx)))
    ([this w ctx b ib]
     (cond
       (map? this)     (write-value-map     this w ctx b ib)
       (seqable? this) (write-value-seqable this w ctx b ib)
       :default        (write-value-default this w ctx b ib))))

  String
  (write-value
    ([this w ctx]
     (w/append w this))
    ([this w ctx block inverted-block]
     (if-not (str/blank? this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  Boolean
  (write-value
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if this
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx)))))
