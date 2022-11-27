(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]
   [darkleaf.web-template.internal.backtick :refer [template-fn]]))

(set! *warn-on-reflection* true)

(defmacro compile [form]
  `(p/compile-element ~(template-fn form)))

(defn render-to-string
  ([template data]
   (render-to-string template nil data))
  ([template ctx data]
   (w/write-to-string
    #(p/render template % (p/ctx-push ctx data)))))

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
           (doseq [item (interpose " " body)]
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

           (doseq [item (interpose " " body)]
             (p/render item w ctx))

           (w/append-raw w "</")
           (w/append w tag)
           (w/append-raw w ">")))))))

(defn- list-element [node]
  (let [key            (nth node 0 nil)
        block          (-> node (nth 1 nil) p/compile-element)
        inverted-block (-> node (nth 2 nil) p/compile-element)
        write          (case (count node)
                         1     p/write
                         (2 3) #(p/write %1 %2 %3 block inverted-block))]
    (reify p/Renderable
      (render [this w ctx]
        (let [value (get ctx key)]
          (write value w ctx))))))

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

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  clojure.lang.Var
  (render [this w ctx]
    (p/render @this w ctx))

  clojure.lang.Fn
  (render [this w ctx]
    (p/render (this ctx) w ctx))

  Object
  (render [this writer _]
    (w/append writer (str this))))

(extend-protocol p/Value
  nil
  (write
    ([this _ _])
    ([this w ctx block inverted-block]
     (p/render inverted-block w ctx)))

  Object
  (write
    ([this w _]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (p/render block w (p/ctx-push ctx this))))

  String
  (write
    ([this w ctx]
     (w/append w this))
    ([this w ctx block inverted-block]
     (if-not (str/blank? this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  Boolean
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if this
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  clojure.lang.Sequential
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (doseq [item this]
         (p/render block w (p/ctx-push ctx item))
         ;; todo? space
         (w/append-raw w " "))
       (p/render inverted-block w ctx))))

  clojure.lang.IPersistentMap
  (write
    ([this w ctx]
     (w/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx)))))
