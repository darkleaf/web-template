(ns darkleaf.web-template.impl.value
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]
   [clojure.string :as str]))

(extend-protocol p/Value
  nil
  (write
    ([this _ _])
    ([this w ctx block inverted-block]
     (p/render inverted-block w ctx)))

  Object
  (write
    ([this w _]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (p/render block w (p/ctx-push ctx this))))

  String
  (write
    ([this w ctx]
     (w/append w this))
    ([this w ctx block inverted-block]
     (if-not (str/blank? this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  Boolean
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if this
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  clojure.lang.Sequential
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (doseq [item this]
         (p/render block w (p/ctx-push ctx item))
         ;; todo? space
         (w/append-raw w " "))
       (p/render inverted-block w ctx))))

  clojure.lang.IPersistentMap
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx)))))
