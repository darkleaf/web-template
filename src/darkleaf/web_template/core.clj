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

;; appendable :: (fn [^Writer w])
;; tmpl :: data -> appendable


(defprotocol Section
  (to-appendable [this ctx block-tmpl inverted-block-tmpl]))

(defn- nil-node [node]
  (when (nil? node)
    (fn [ctx]
      (fn [^Writer w]))))

(defn- string-node [node]
  (when (string? node)
    (fn [ctx]
      (fn [^Writer w]
        (.append w ^String node)))))

(defn- static-tag--no-attrs--body [[tag & body :as node]]
  (when (and (vector? node)
             (ident? tag)
             (not (-> body first map?)))
    (let [tag  (name tag)
          body (map compile body)]
      (fn [ctx]
        (fn [^Writer w]
          (.append w "<")
          (.append w tag)
          (.append w ">")

          (doseq [item body]
            ;; todo: fix
            ((item ctx) w))

          (.append w "</")
          (.append w tag)
          (.append w ">"))))))

(defn- dynamic-node [[key block inverted-block :as node]]
  (when (list? node)
    (let [block          (compile block)
          inverted-block (compile inverted-block)]
      (fn [ctx]
        (let [value (get ctx key)
              ctx   (ctx-push ctx value)]
          (to-appendable value ctx block inverted-block))))))

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
    ((template ctx) sw)
    (.toString sw)))

(comment
  '[div {}
    ""
    [div {}]])

(extend-protocol Section
  String
  (to-appendable [this _ _ _]
    (fn [^Writer w]
      (.append w this)))

  Boolean
  (to-appendable [this ctx block-tmpl inverted-block-tmpl]
    (if this
      (block-tmpl ctx)
      (inverted-block-tmpl ctx)))

  ;; todo
  clojure.lang.PersistentVector
  ;; todo: empty
  (to-appendable [this ctx block-tmpl inverted-block-tmpl]
    ;; parent-ctx как раз можно в inverted передавать
    (let [appendables (for [item this ;; оно ленивое
                            :let [ctx (ctx-push ctx item)]]
                        (block-tmpl ctx))]
      (fn [^Writer w]
        (doseq [appendable appendables]
          (appendable w)))))

  clojure.lang.PersistentArrayMap
  (to-appendable [this ctx block-tmpl inverted-block-tmpl]
    (block-tmpl ctx)))
