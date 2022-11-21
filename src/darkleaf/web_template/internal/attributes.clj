(ns darkleaf.web-template.internal.attributes
  (:require
   [clojure.string :as str]
   [clojure.walk :as w]
   [darkleaf.web-template.internal.utils :as u]))

(defn- attr-name [k]
  (->> [(u/namespace k) (name k)]
       (u/join-some ":")))

(defn- update-value [ctx value patch]
  (cond
    (nil? patch)     nil
    (false? patch)   nil
    (true? patch)    true
    (string? patch)  patch
    (ident? patch)   (name patch)
    (map? patch)     (->> patch
                          (filter val)
                          (map key)
                          (map name)
                          (u/cons-some value)
                          (str/join " "))
    (seqable? patch) (->> patch
                          (filter some?)
                          (map name)
                          (u/cons-some value)
                          (str/join " "))
    (fn? patch)      (recur ctx value (patch ctx))
    :else            (str patch)))

(defn- update-value-r [ctx acc proto-k patch]
  (let [k (attr-name proto-k)
        v (get acc k)]
    (if-some [v (update-value ctx v patch)]
      (assoc acc k v)
      (dissoc acc k))))

(defn- ctx-resolve [ctx node]
  (if (list? node)
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

(defn merge-attrs [literal attrs ctx]
  (let [attrs          (resolve-attrs ctx attrs)
        dynamic        (get attrs '...)
        attrs          (dissoc attrs '...)
        update-value-r (partial update-value-r ctx)
        res            (reduce-kv update-value-r literal attrs)
        res            (reduce-kv update-value-r res dynamic)]
    res))
