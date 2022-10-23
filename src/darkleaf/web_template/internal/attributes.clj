(ns darkleaf.web-template.internal.attributes
  (:require [clojure.string :as str]))

(defn- attr-name [k]
  (let [attr-ns   (if (ident? k) (namespace k))
        attr-name (name k)]
    (if attr-ns
      (str attr-ns ":" attr-name)
      attr-name)))

(defn- cons-some [x seq]
  (if x
    (cons x seq)
    seq))

(defn- add-value* [ctx acc k v]
  (let [k (attr-name k)]
    (cond
      (nil? v)     acc
      (false? v)   (dissoc acc k)
      (true? v)    (assoc acc k true)
      (string? v)  (assoc acc k v)
      (ident? v)   (assoc acc k (name v))
      (map? v)     (update acc k #(->> v
                                       (filter val)
                                       (map key)
                                       (map name)
                                       (cons-some %)
                                       (str/join " ")))
      (seqable? v) (update acc k #(->> v
                                       (map name)
                                       (cons-some %)
                                       (str/join " ")))
      :else        (assoc acc k (str v)))))

(defn- add-value [ctx acc k v]
  (let [v (if (list? v)
            (get ctx (first v))
            v)]
    (add-value* ctx acc k v)))

(defn merge-attrs [literal attrs ctx]
  (let [dynamic-key (get attrs '...)
        dynamic     (if dynamic-key (get ctx dynamic-key))
        attrs       (dissoc attrs '...)
        add-value   (partial add-value ctx)
        res         (reduce-kv add-value literal attrs)
        res         (reduce-kv add-value res dynamic)]
    res))
