(ns darkleaf.web-template.impl.attribute-value
  (:require
   [clojure.string :as str]
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
  (update-attribute-value [patch ctx value]
    (let [patch (->> patch
                     (filter val)
                     (map key))]
      (p/update-attribute-value patch ctx value)))

  java.util.Collection
  (update-attribute-value [patch _ value]
    (->> patch
         (cons value)
         (filter some?)
         (map name)
         (str/join " ")))

  clojure.lang.Fn
  (update-attribute-value [patch ctx value]
    (p/update-attribute-value (patch ctx) ctx value)))
