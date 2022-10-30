(ns darkleaf.web-template.internal.attributes
  (:require [clojure.string :as str]))

(defn- attr-name [k]
  (let [attr-ns   (if (ident? k) (namespace k))
        attr-name (name k)]
    (if attr-ns
      (str attr-ns ":" attr-name)
      attr-name)))

(defn- cons-some [x seq]
  (if (some? x)
    (cons x seq)
    seq))

(defn- add-value [ctx acc k v]
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
                                       (filter some?)
                                       (map name)
                                       (cons-some %)
                                       (str/join " ")))
      :else        (assoc acc k (str v)))))

;; todo? default
;; [div {class (:class default-a default-b ...)} ...]
(defn- resolve-attr [ctx acc k v]
  (let [k (if (list? k)
            (get ctx (first k))
            k)
        v (if (list? v)
            (get ctx (first v))
            v)]
    (assoc acc k v)))

(defn resolve-attrs [ctx attrs]
  (reduce-kv (partial resolve-attr ctx)
             {} attrs))

(defn merge-attrs [literal attrs ctx]
  (let [attrs     (resolve-attrs ctx attrs)
        dynamic   (get attrs '...)
        attrs     (dissoc attrs '...)
        add-value (partial add-value ctx)
        res       (reduce-kv add-value literal attrs)
        res       (reduce-kv add-value res dynamic)]
    res))
