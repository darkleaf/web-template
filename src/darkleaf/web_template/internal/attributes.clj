(ns darkleaf.web-template.internal.attributes
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]
   [darkleaf.web-template.internal.utils :as u]))

(defn- attr-name [k]
  (u/join-some ":" [(u/namespace k) (name k)]))

(defn- add-value [ctx acc k v]
  (let [k (attr-name k)]
    (cond
      (nil? v)     (dissoc acc k)
      (false? v)   (dissoc acc k)
      (true? v)    (assoc acc k true)
      (string? v)  (assoc acc k v)
      (ident? v)   (assoc acc k (name v))
      (map? v)     (update acc k #(->> v
                                       (filter val)
                                       (map key)
                                       (map name)
                                       (u/cons-some %)
                                       (str/join " ")))
      (seqable? v) (update acc k #(->> v
                                       (filter some?)
                                       (map name)
                                       (u/cons-some %)
                                       (str/join " ")))
      (fn? v)      (recur ctx acc k (v ctx))
      :else        (assoc acc k (str v)))))

(defn- ctx-resolve [ctx node]
  (if (list? node)
    (get ctx (first node))
    node))

(defn- resolve-attr [ctx acc k v]
  (let [ctx-resolve (partial ctx-resolve ctx)
        k           (w/prewalk ctx-resolve k)
        v           (w/prewalk ctx-resolve v)]
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
