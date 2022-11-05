(ns darkleaf.web-template.core
  #_(:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [darkleaf.web-template.internal.attributes :refer [resolve-attrs merge-attrs]]
   [clojure.walk :as w]
   [clojure.test :as t])
  (:import
   (java.io StringWriter)))

(set! *warn-on-reflection* true)

(defn render-to-string
  ([template data]
   (render-to-string template nil data))
  ([template ctx data]
   (let [sw (StringWriter.)]
     (p/render template sw (p/ctx-push ctx data))
     (.toString sw))))

(defmacro chain-handlers
  {:private      true
   :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

(defn- vector-<>-element [[tag & body :as node]]
   (when (= '<> tag)
     (let [body (map p/compile body)]
       (reify p/Renderable
         (render [_ w ctx]
           (doseq [item (interpose " " body)]
             (p/render item w ctx)))))))

(defn- vector-tag-element [[tag :as node]]
  (when (ident? tag) ;; todo? string
    (let [attrs?              (map? (nth node 1 nil))
          attrs               (if attrs? (nth node 1 nil))
          body                (nthnext node (if attrs? 2 1))
          [tag literal-attrs] (parse-tag tag)
          tag                 ^String tag
          body                (mapv p/compile body)]
      (reify p/Renderable
        (render [_ w ctx]
        ;; todo: добавить случай для литеральных атрибутов,
        ;; чтобы не мержжить все это в рантайме
         (let [attrs (merge-attrs literal-attrs attrs ctx)]
           (p/append-raw w "<")
           (p/append w tag)  ;; todo? [(:tag) {} ...]
           (doseq [[attr value] attrs]
             (p/append-raw w " ")
             ;; todo: value = true
             (p/append w attr)
             (p/append-raw w "=\"")
             (p/append w (str value))
             (p/append-raw w "\""))
           (p/append-raw w ">")

           (doseq [item (interpose " " body)]
             (p/render item w ctx))

           (p/append-raw w "</")
           (p/append w tag)
           (p/append-raw w ">")))))))

(defn- list-element [node]
  (let [key            (nth node 0 nil)
        block          (-> node (nth 1 nil) p/compile)
        inverted-block (-> node (nth 2 nil) p/compile)
        write          (case (count node)
                         1     p/write
                         (2 3) #(p/write %1 %2 %3 block inverted-block))]
    (reify p/Renderable
      (render [this w ctx]
        (let [value (get ctx key)]
          (write value w ctx))))))

(extend-protocol p/Element
  nil
  (compile [this] this)

  Object
  (compile [this] this)

  clojure.lang.PersistentVector
  (compile [this]
    (chain-handlers this
      vector-<>-element
      vector-tag-element
      #_todo-else))

  clojure.lang.PersistentList
  (compile [this]
    (chain-handlers this
      list-element)))

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  ;; todo: literal string

  Object
  (render [this writer _]
    (p/append writer (str this))))

(extend-protocol p/Value
  nil
  (write
    ([this _ _])
    ([this w ctx block inverted-block]
     (p/render inverted-block w ctx)))

  Object
  (write
    ([this w _]
     (p/append w (str this)))
    ([this w ctx block inverted-block]
     (p/render block w (p/ctx-push ctx this))))

  String
  (write
    ([this w ctx]
     (p/append w this))
    ([this w ctx block inverted-block]
     (if-not (str/blank? this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  Boolean
  (write
    ([this w ctx]
     (p/append w (str this)))
    ([this w ctx block inverted-block]
     (if this
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx))))

  clojure.lang.Sequential
  (write
    ([this w ctx]
     (p/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (doseq [item this]
         (p/render block w (p/ctx-push ctx item))
         ;; todo? space
         (p/append-raw w " "))
       (p/render inverted-block w ctx))))

  clojure.lang.IPersistentMap
  (write
    ([this w ctx]
     (p/append w (str this)))
    ([this w ctx block inverted-block]
     (if (seq this)
       (p/render block w (p/ctx-push ctx this))
       (p/render inverted-block w ctx)))))




(comment
 (declare compile*)



 (def ^:private space
   (template [w ctx]
     (p/append w " ")))

 (defn- nil-node [node]
   (when (nil? node)
     (template [_ _])))

 (declare dynamic-node)

 (defn- dot-node [node]
   (when (= '. node)
     (dynamic-node '(.))))

 (defn- string-node [node]
   (when (string? node)
     (template [w _]
       (p/append w node))))

 (defn- number-node [node]
   (when (number? node)
     (template [w _]
       (p/append w (str node)))))




 (defn compile* [node]
   (chain-handlers node
     nil-node
     string-node
     number-node
     dot-node
     dynamic-node
     <>-node
     tag-node))

 (defmacro compile [node]
   `(compile* (quote ~node)))


 (extend-protocol p/Component
   nil
   (render [this _ _])
   (render-present [_ _ _ _])
   (render-blank [this w ctx block]
     (p/render-tmpl block w (p/ctx-push ctx this)))

   Object
   (render [this w _]
     ;; todo: escape
     (p/append w (str this)))
   (render-present [this w ctx block]
     (p/render-tmpl block w (p/ctx-push ctx this)))
   (render-blank [_ _ _ _])

   String
   (render [this w ctx]
     ;; todo: escape
     (p/append w this))
   (render-present [this w ctx block]
     (if-not (str/blank? this)
       (p/render-tmpl block w (p/ctx-push ctx this))))
   (render-blank [this w ctx block]
     (if (str/blank? this)
       (p/render-tmpl block w (p/ctx-push ctx this))))

   Boolean
   (render [this w ctx]
     (p/append w (str this)))
   (render-present [this w ctx block]
     (if this
       (p/render-tmpl block w (p/ctx-push ctx this))))
   (render-blank [this w ctx block]
     (if-not this
       (p/render-tmpl block w ctx)))

   clojure.lang.Sequential
   (render [this w ctx]
     ;; todo: escape
     (p/append w (str this)))
   (render-present [this w ctx block]
     (if (seq this)
       (doseq [item this]
         (p/render-tmpl block w (p/ctx-push ctx item))
         (p/append w " "))))
   (render-blank [this w ctx block]
     (if (empty? this)
       (p/render-tmpl block w ctx)))

   clojure.lang.IPersistentMap
   (render [this w ctx]
     ;; todo: escape
     (p/append w (str this)))
   (render-present [this w ctx block]
     (if (seq this)
       (p/render-tmpl block w (p/ctx-push ctx this))))
   (render-blank [this w ctx block]
     (if (empty? this)
       (p/render-tmpl block w ctx)))))
