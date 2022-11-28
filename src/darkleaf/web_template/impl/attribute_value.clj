(ns darkleaf.web-template.impl.attribute-value
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]))

(defn update-attribute-value-seqable [patch _ value]
  (->> patch
       (cons value)
       (filter some?)
       (map name)
       (str/join " ")))

(defn update-attribute-value-map [patch ctx value]
  (let [patch (->> patch
                   (filter val)
                   (map key))]
    (update-attribute-value-seqable patch ctx value)))

(defn update-attribute-value-ident [patch _ _]
  (name patch))

(defn update-attribute-value-ifn [patch ctx value]
  (p/update-attribute-value (patch ctx) ctx value))

(defn update-attribute-value-default [patch _ _]
  (str patch))

(extend-protocol p/AttributeValue
  nil
  (update-attribute-value [_ _ _]
    nil)

  Object
  (update-attribute-value [patch ctx value]
    (cond
      (ident? patch)   (update-attribute-value-ident   patch ctx value)
      (map? patch)     (update-attribute-value-map     patch ctx value)
      (seqable? patch) (update-attribute-value-seqable patch ctx value)
      (ifn? patch)     (update-attribute-value-ifn     patch ctx value)
      :default         (update-attribute-value-default patch ctx value)))

  Boolean
  (update-attribute-value [patch _ _]
    (if patch
      true
      nil))

  String
  (update-attribute-value [patch _ _]
    patch))
