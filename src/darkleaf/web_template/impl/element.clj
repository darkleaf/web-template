(ns darkleaf.web-template.impl.element
  (:require
   [darkleaf.web-template.context :as ctx]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defmacro chain-handlers
  {:private      true
   :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

(defn- vector-<>-element [[tag & body :as node]]
   (when (= '<> tag)
     (let [body (map p/compile-element body)]
       (reify p/Renderable
         (render [_ w ctx]
           (doseq [item body]
             (p/render item w ctx)))))))

(defn- tag? [tag]
  (or (ident? tag)
      (string? tag)))

(defn- vector-tag-element [[tag :as node]]
  (when (tag? tag)
    (let [attrs?              (map? (nth node 1 nil))
          attrs               (if attrs? (nth node 1 nil))
          body                (nthnext node (if attrs? 2 1))
          [tag literal-attrs] (parse-tag tag)
          tag                 ^String tag
          body                (mapv p/compile-element body)]
      (reify p/Renderable
        (render [_ w ctx]
        ;; todo: добавить случай для литеральных атрибутов,
        ;; чтобы не мержжить все это в рантайме
         (let [attrs (merge-attrs literal-attrs attrs ctx)]
           (w/append-raw w "<")
           (w/append w tag)  ;; todo? [(:tag) {} ...]
           (doseq [[attr value] attrs]
             (w/append-raw w " ")
             ;; todo: value = true
             (w/append w attr)
             (w/append-raw w "=\"")
             (w/append w (str value))
             (w/append-raw w "\""))
           (w/append-raw w ">")

           (doseq [item body]
             (p/render item w ctx))

           (w/append-raw w "</")
           (w/append w tag)
           (w/append-raw w ">")))))))

(defn- list-element [[key block inverted-block :as node]]
  (let [block          (-> block          p/compile-element)
        inverted-block (-> inverted-block p/compile-element)
        write-value    (case (count node)
                         1     p/write-value
                         (2 3) #(p/write-value %1 %2 %3 block inverted-block))
        get-value      (if (= 'this key)
                         #(ctx/this %)
                         #(get % key))]
    (reify p/Renderable
      (render [this w ctx]
        (let [value (get-value ctx)]
          (write-value value w ctx))))))

(extend-protocol p/Element
  ;; todo: literal string
  #_#_
  String
  (compile [this]
    (reify p/Renderable ...))
  ;; Если делать через extend-protocol p/Renderable,
  ;; то нет уверености в безопасности этой строки.
  ;; Она может прилететь из хэлпера.

  nil
  (compile-element [this] this)

  Object
  (compile-element [this] this)

  clojure.lang.PersistentVector
  (compile-element [this]
    (chain-handlers this
      vector-<>-element
      vector-tag-element
      #_todo-else))

  clojure.lang.PersistentList
  (compile-element [this]
    (chain-handlers this
      list-element)))
