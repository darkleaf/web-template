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
  [node-binding mode & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding ~mode))))

(defn- vector-<>-element [[tag & body :as node] mode]
   (when (= '<> tag)
     (let [body (map #(p/element->renderable % mode) body)]
       (reify p/Renderable
         (render [_ w ctx]
           (doseq [item body]
             (p/render item w ctx)))))))

(defn- tag? [tag]
  (or (ident? tag)
      (string? tag)))

(defn- xml-attribute [w name value]
  (w/append w name)
  (w/append-raw w "=\"")
  (w/append w value)
  (w/append-raw w "\""))

(defn- empty-attribute [w name]
  (w/append w name))

(defn- write-attribute [w name value {:keys [empty-attributes]}]
  (if (true? value)
    (if empty-attributes
      (empty-attribute w name)
      (xml-attribute w name name))
    (xml-attribute w name value)))

(defn- vector-tag-element [[tag :as node]
                           {:keys [void-elements] :as mode}]
  (when (tag? tag)
    (let [attrs?              (map? (nth node 1 nil))
          attrs               (if attrs? (nth node 1 nil))
          body                (nthnext node (if attrs? 2 1))
          [tag literal-attrs] (parse-tag tag)
          tag                 ^String tag
          body                (mapv #(p/element->renderable % mode) body)]
      (reify p/Renderable
        (render [_ w ctx]
        ;; todo: добавить случай для литеральных атрибутов,
        ;; чтобы не мержжить все это в рантайме
         (let [attrs (merge-attrs literal-attrs attrs ctx)]
           (w/append-raw w "<")
           (w/append w tag)
           (doseq [[attr value] attrs]
             (w/append-raw w " ")
             (write-attribute w attr value mode))
           (w/append-raw w ">")
           ;; todo: in compile time
           (when-not (void-elements tag)
             (doseq [item body]
               (p/render item w ctx))

             (w/append-raw w "</")
             (w/append w tag)
             (w/append-raw w ">"))))))))

(defn- list-element [[key block inverted-block :as node] mode]
  (let [block          (-> block          (p/element->renderable mode))
        inverted-block (-> inverted-block (p/element->renderable mode))
        to-renderable  (case (count node)
                         1     identity
                         (2 3) #(p/container->renderable % block inverted-block))
        get-value      (if (= 'this key)
                         #(ctx/this %)
                         #(get % key))]
    (reify p/Renderable
      (render [this w ctx]
        (let [value      (get-value ctx)
              renderable (to-renderable value)]
          (p/render renderable w ctx))))))

(extend-protocol p/Element
  nil
  (element->renderable [this _] this)

  Object
  (element->renderable [this _] this)

  String
  (element->renderable [this _]
    ;; todo: wrap with RawString
    (reify p/Renderable
      (render [_ w _]
        (w/append-raw w this))))

  clojure.lang.PersistentVector
  (element->renderable [this mode]
    (chain-handlers this mode
      vector-<>-element
      vector-tag-element
      #_todo-else))

  clojure.lang.PersistentList
  (element->renderable [this mode]
    (chain-handlers this mode
      list-element)))
