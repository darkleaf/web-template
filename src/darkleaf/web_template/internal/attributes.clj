(ns darkleaf.web-template.internal.attributes
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]
   [darkleaf.web-template.protocols :as p]))

(defn- attr-name [k]
  (if (qualified-ident? k)
    (str (namespace k) ":" (name k))
    (name k)))

(defn- ctx-resolve [ctx node]
  (if (list? node)
    ;; todo: ctx/get
    (get ctx (first node))
    node))

(defn- resolve-attr [ctx acc k v]
  (let [ctx-resolve (partial ctx-resolve ctx)
        k           (w/prewalk ctx-resolve k)
        v           (w/prewalk ctx-resolve v)]
    (assoc acc k v)))

(defn- resolve-attrs [ctx attrs]
  (reduce-kv (partial resolve-attr ctx)
             {} attrs))

(defn- normalize-attrs [attrs]
  (->> attrs
       (reduce-kv (fn [acc proto-k proto-v]
                    (let [k (attr-name proto-k)
                          v (p/attribute-value proto-v)]
                      (if v
                        (assoc! acc k v)
                        acc)))
                  (transient {}))
       persistent!))

(defn merge-attrs [literal attrs ctx]
  (let [attrs   (resolve-attrs ctx attrs)
        dynamic (get attrs '...)
        attrs   (dissoc attrs '...)
        attrs   (normalize-attrs attrs)
        dynamic (normalize-attrs dynamic)]
    (merge-with #(str %1 " " %2)
                literal
                attrs
                dynamic)))
