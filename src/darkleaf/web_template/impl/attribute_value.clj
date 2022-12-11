(ns darkleaf.web-template.impl.attribute-value
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]))

(defn attribute-value-seqable [this ctx]
  (->> this
       (filter some?)
       (map #(p/attribute-value % ctx))
       (str/join " ")))

(defn attribute-value-map [this ctx]
  (let [this (->> this
                  (filter val)
                  (map key))]
    (attribute-value-seqable this ctx)))

(defn attribute-value-ident [this _]
  ;; todo? foo/bar -> foo--bar, stimulus
  (name this))

(defn attribute-value-ifn [this ctx]
  (p/attribute-value (this ctx) ctx))

(defn attribute-value-default [this _]
  (str this))

(extend-protocol p/AttributeValue
  nil
  (attribute-value [_ _]
    nil)

  Object
  (attribute-value [this ctx]
    (cond
      (ident? this)   (attribute-value-ident   this ctx)
      (map? this)     (attribute-value-map     this ctx)
      (seqable? this) (attribute-value-seqable this ctx)
      (ifn? this)     (attribute-value-ifn     this ctx)
      :default        (attribute-value-default this ctx)))

  Boolean
  (attribute-value [this _]
    (when this
      true))

  String
  (attribute-value [this _]
    this))
