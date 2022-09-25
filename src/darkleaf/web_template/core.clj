(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:import
   (java.io Writer StringWriter)))

(set! *warn-on-reflection* true)

(declare compile)

(defn- ctx-push [ctx v]
  (merge ctx
         (if (map? v) v)
         {'. v}))

(comment
  [tag & body]
  [tag attributes & body])

;; tmpl :: w data -> ()


(defprotocol Section
  (write [this ^Writer writer ctx block-tmpl inverted-block-tmpl]))

(defn- nil-node [node]
  (when (nil? node)
    (fn [_ _])))

(defn- string-node [node]
  (when (string? node)
    (fn [^Writer w ctx]
      (.append w ^String node))))

(defn- static-tag--no-attrs--body [[tag & body :as node]]
  (when (and (vector? node)
             (ident? tag)
             (not (-> body first map?)))
    (let [tag  (name tag)
          body (map compile body)]
      (fn [^Writer w ctx]
        (.append w "<")
        (.append w tag)
        (.append w ">")

        (doseq [item body]
          (item w ctx))

        (.append w "</")
        (.append w tag)
        (.append w ">")))))

(defn- dynamic-node [[key block inverted-block :as node]]
  (when (list? node)
    (let [block          (compile block)
          inverted-block (compile inverted-block)]
      (fn [w ctx]
        (let [value (get ctx key)
              ctx   (ctx-push ctx value)]
          (write value w ctx block inverted-block))))))

(defmacro chain-handlers
  {:private true :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

(defn compile [node]
  (chain-handlers node
    nil-node
    string-node
    dynamic-node
    static-tag--no-attrs--body))



(defn render-to-string [template data]
  (let [sw  (StringWriter.)
        ctx (ctx-push nil data)]
    (template sw ctx)
    (.toString sw)))

(comment
  '[div {}
    ""
    [div {}]])

(extend-protocol Section
  String
  (write [this ^Writer w _ _ _]
    (.append w this))

  Boolean
  (write [this w ctx block-tmpl inverted-block-tmpl]
    (if this
      (block-tmpl w ctx)
      (inverted-block-tmpl w ctx)))

  ;; todo
  clojure.lang.PersistentVector
  ;; todo: empty
  (write [this w ctx block-tmpl inverted-block-tmpl]
    (doseq [item this
            :let [ctx (ctx-push ctx item)]]
      (block-tmpl w ctx)))

  clojure.lang.PersistentArrayMap
  (write [this w ctx block-tmpl inverted-block-tmpl]
    (block-tmpl w ctx)))
