(ns darkleaf.web-template.impl.attribute-value
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.internal.utils :as u]
   [darkleaf.web-template.protocols :as p]))

(extend-protocol p/AttributeValue
  nil
  (update-attribute-value [_ _ _]
    nil)

  Object
  (update-attribute-value [patch _ _]
    (str patch))

  Boolean
  (update-attribute-value [patch _ _]
    (if (true? patch)
      true
      nil))

  String
  (update-attribute-value [patch _ _]
    patch)

  clojure.lang.Named
  (update-attribute-value [patch _ _]
    (name patch))

  clojure.lang.IPersistentMap
  (update-attribute-value [patch _ value]
    (->> patch
         (filter val)
         (map key)
         (map name)
         (u/cons-some value)
         (str/join " ")))

  java.util.Collection
  (update-attribute-value [patch _ value]
    (->> patch
         (filter some?)
         (map name)
         (u/cons-some value)
         (str/join " ")))

  clojure.lang.Fn
  (update-attribute-value [patch ctx value]
    (p/update-attribute-value (patch ctx) ctx value)))
