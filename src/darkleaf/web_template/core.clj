(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]])
  (:import
   (java.io Writer StringWriter)
   (darkleaf.web_template.protocols Template)))

(set! *warn-on-reflection* true)

(comment
  [tag & body]
  [tag {} & body]
  [.class.klass#id {. :attrs, class "xyz"} & body]

  (:key (:attrs) nil nil)
  (:key (:attrs) block nil)
  (:key (:attrs) block inverted-block)

  (:key {. :attrs})

  [div {class (:class)}]
  [div {(:attr) true}]
  [div {. :attrs}]
  [div {data {foo "43"}}]

  [div {class (:class "default")}]

  [.foo#bar {data {(:segment) {foo   (:x)
                               (:y)  "bar"
                               "xyz" "42"}}}]

  (:format {:value :my-value :format "%.2f"})
  (:debug {:value :my-value}) ;; <pre> pprint
  (:raw {:value :my-value})

  (:panel-component {:title [.title (:title)]}
                    "content"
                    "empty")

  ;; чтобы можно было делать так:
  [<>
   "<!DOCTYPE html>"
   [html
    [head ...]
    [body
     (:body-component {} . "not found")]]])

(declare compile)

(defn ctx-push [ctx v]
  (merge ctx
         (if (map? v) v)
         {'. v}))

(def ^:private space
  (reify Template
    (render [_ w ctx]
      (.append w " "))))

(defn- nil-node [node]
  (when (nil? node)
    (reify Template
      (render [_ _ _]))))

(declare dynamic-node)

(defn- dot-node [node]
  (when (= '. node)
    (dynamic-node '(.))))

(defn- string-node [node]
  (when (string? node)
    (reify Template
      (render [_ w ctx]
        (.append w ^String node)))))

(defn- <>-node [[tag & body :as node]]
  (when (and (vector? node)
             (= '<> tag))
    (let [body (map compile body)]
      (reify Template
        (render [_ w ctx]
          (doseq [^Template item (interpose space body)]
            (.render item w ctx)))))))

(defn- tag-node [[tag :as node]]
  (when (and (vector? node)
             (ident? tag))
    (let [attrs?              (map? (nth node 1 nil))
          attrs               (if attrs? (nth node 1 nil))
          body                (nthnext node (if attrs? 2 1))
          [tag literal-attrs] (parse-tag tag)
          tag                 ^String tag
          body                (mapv compile body)]
      (reify Template
        (render [_ w ctx]
          ;; todo: добавить случай для литеральных атрибутов,
          ;; чтобы не мержжить все это в рантайме
          (let [attrs (merge-attrs literal-attrs attrs ctx)]
            (.append w "<")
            (.append w tag)
            (doseq [[^String attr value] attrs]
              (.append w " ")
              ;; todo: value = true
              ;; todo: escape
              (.append w attr)
              (.append w "=\"")
              (.append w (str value))
              (.append w "\""))
            (.append w ">")

            (doseq [^Template item (interpose space body)]
              (.render item w ctx))

            (.append w "</")
            (.append w tag)
            (.append w ">")))))))

(defn- compile-attr-value [v]
  (if (vector? v)
    (compile v)
    v))

(defn- dynamic-node [node]
  (when (list? node)
    (let [key            (nth node 0 nil)
          attrs?         (map? (nth node 1 nil))
          attrs          (if attrs? (nth node 1 nil))
          attrs          (update-vals attrs compile-attr-value)
          block          (-> node (nth (if attrs? 2 1) nil) compile)
          inverted-block (-> node (nth (if attrs? 3 2) nil) compile)
          render'        (if (= (if attrs? 2 1)
                                (count node))
                           p/render
                           #(p/render %1 %2 %3 %4 block inverted-block))]
      (reify Template
        (render [_ w ctx]
          (let [value (get ctx key)
                ctx   (ctx-push ctx value)]
            (render' value w ctx attrs)))))))

(defmacro chain-handlers
  {:private true :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

(defn compile [node]
  (chain-handlers node
    nil-node
    string-node
    dot-node
    dynamic-node
    <>-node
    tag-node))

(defn render-to-string [^Template template data]
  (let [sw  (StringWriter.)
        ctx (ctx-push nil data)]
    (.render template sw ctx)
    (.toString sw)))

(extend-protocol p/Component
  Template
  (render
    ([this ^Writer w ctx attrs]
     (.render this w (merge ctx attrs)))
    ([this w ctx attrs block inverted-block]
     (p/render this w
               (assoc ctx :block block :inverted-block inverted-block)
               attrs)))

  nil
  (render
    ([this _ _ _])
    ([_ w ctx attrs _ ^Template inverted-block]
     (.render inverted-block w ctx)))

  Object
  (render
    ([this ^Writer w _ _]
     ;; todo: escape
     (.append w (str this)))
    ([_ w ctx attrs ^Template block _]
     (.render block w ctx)))

  String
  (render
    ([this ^Writer w ctx attrs]
     ;; todo: escape
     (.append w this))
    ([this w ctx attrs ^Template block ^Template inverted-block]
     (if-not (str/blank? this)
       (.render block w ctx)
       (.render inverted-block w ctx))))

  Boolean
  (render
    ([this ^Writer w ctx attrs]
     (.append w (str this)))
    ([this w ctx attrs ^Template block ^Template inverted-block]
     (if this
       (.render block w ctx)
       (.render inverted-block w ctx))))

  clojure.lang.Sequential
  (render
    ([this ^Writer w ctx attrs]
     ;; todo: escape
     (.append w (str this)))
    ([this ^Writer w ctx attrs ^Template block ^Template inverted-block]
     (if (seq this)
       (let [separator (-> attrs (get :separator " ") str)
             separator #(.append w separator)]
         (doseq [tmpl (interpose separator
                                 (for [item this
                                       :let [ctx (ctx-push ctx item)]]
                                   #(.render block w ctx)))]
            (tmpl)))
       (.render inverted-block w ctx))))

  clojure.lang.IPersistentMap
  (render
    ([this ^Writer w ctx attrs]
     ;; todo: escape
     (.append w (str this)))
    ([this w ctx attrs ^Template block ^Template inverted-block]
     (if (seq this)
       (.render block w ctx)
       (.render inverted-block w ctx)))))
