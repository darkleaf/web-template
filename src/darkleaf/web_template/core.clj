(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [darkleaf.web-template.internal.attributes :refer [resolve-attrs merge-attrs]])
  (:import
   (java.io StringWriter)))

(set! *warn-on-reflection* true)

(comment
  [tag & body]
  [tag {} & body]
  [.class.klass#id {. :attrs, class "xyz"} & body]


  ;; object template через интерфейс, а не компилировать


  (:tags)
  (:tags {:separator ^:compile [br]} .)
  (:tags . [br])
  ^:blank (:tags "no tags")

  (component [this attrs & body]
    (when ...
      ...)
    [:div ...])
  (:component {} :a :b :c :d)


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
     (:body-component {} . "not found")]]]

  (component [attrs ctx]
    (update ctx ....)
    [div ...])
  (component* (fn [ctx] ctx) tmpl)

  ;; (fn [attrs ctx] -> ctx) as component
  ;; (assoc ctx '. ...)


  (fn [ctx args]
    (-> ctx
        (p/ctx-push {:a 1 :b 2})
        (p/ctx-push tmpl)))

  (ctx, args) -> [ctx' obj]



  [args? block & {:as block-args}]
  (:foo
   {:a 1}
   [div block]
   :b [div block]
   :b [div block])

  (:tags {:separator ", "} .)
  (:tags . :separator ", ")
  (:tags . :separator [br])

  ^:blank (:tags "no tags")


  (compile [div])
  (compile* '[div]))

(declare compile*)

(defmacro template
  {:style/indent :defn :private true}
  [[writer ctx] & body]
  `(reify
     p/Template
     (render-tmpl [this# ~writer ~ctx] ~@body)
     p/Component
     (render [this# w# ctx# attrs#]
       (p/render-tmpl this# w# (merge ctx# attrs#)))
     (render [this# w# ctx# attrs# block# inverted-block#]
       (p/render-tmpl this# w# (merge ctx# attrs# {:block block#
                                                   :inverted-block inverted-block#})))))

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

(defn- <>-node [[tag & body :as node]]
  (when (and (vector? node)
             (= '<> tag))
    (let [body (map compile* body)]
      (template [w ctx]
        (doseq [item (interpose space body)]
          (p/render-tmpl item w ctx))))))

(defn- tag-node [[tag :as node]]
  (when (and (vector? node)
             (ident? tag))
    (let [attrs?              (map? (nth node 1 nil))
          attrs               (if attrs? (nth node 1 nil))
          body                (nthnext node (if attrs? 2 1))
          [tag literal-attrs] (parse-tag tag)
          tag                 ^String tag
          body                (mapv compile* body)]
      (template [w ctx]
        ;; todo: добавить случай для литеральных атрибутов,
        ;; чтобы не мержжить все это в рантайме
        (let [attrs (merge-attrs literal-attrs attrs ctx)]
          (p/append w "<")
          (p/append w tag)
          (doseq [[attr value] attrs]
            (p/append w " ")
            ;; todo: value = true
            ;; todo: escape
            (p/append w attr)
            (p/append w "=\"")
            (p/append w (str value))
            (p/append w "\""))
          (p/append w ">")

          (doseq [item (interpose space body)]
            (p/render-tmpl item w ctx))

          (p/append w "</")
          (p/append w tag)
          (p/append w ">"))))))

(defn- compile*-attr-value [v]
  (if (-> v meta :tmpl)
    (compile* v)
    v))

(defn- dynamic-node [node]
  (when (list? node)
    (let [key            (nth node 0 nil)
          attrs?         (map? (nth node 1 nil))
          attrs          (if attrs? (nth node 1 nil))
          attrs          (update-vals attrs compile*-attr-value)
          block          (-> node (nth (if attrs? 2 1) nil) compile*)
          inverted-block (-> node (nth (if attrs? 3 2) nil) compile*)
          render         (if (= (if attrs? 2 1)
                                (count node))
                           p/render
                           #(p/render %1 %2 %3 %4 block inverted-block))]
      (template [w ctx]
        (let [component (get ctx key)
              attrs     (resolve-attrs ctx attrs)]
          (render component w ctx attrs))))))

(defmacro chain-handlers
  {:private true :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

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

(defn component* [f tmpl]
  (reify
    p/Component
    (render [_ w ctx attrs]
      (p/render-tmpl tmpl w (-> ctx
                                (p/ctx-push (f ctx attrs))
                                (merge attrs))))
    ;; если nil, то рендерить inverted-block?
    (render [_ w ctx attrs block inverted-block]
      (p/render-tmpl tmpl w (-> ctx
                                (p/ctx-push (f ctx attrs))
                                (merge attrs)
                                (assoc :block block :inverted-block inverted-block))))))

(defmacro component
  {:style/indent :defn}
  [[ctx attrs] body node]
  `(component*
    (fn [~ctx ~attrs] ~body)
    (compile ~node)))

(defn render-to-string
  ([template data] (render-to-string template nil data))
  ([template ctx data]
   (let [sw  (StringWriter.)]
     (p/render-tmpl template sw (p/ctx-push ctx data))
     (.toString sw))))

(extend-protocol p/Component
  nil
  (render
    ([this _ _ _])
    ([this w ctx attrs _ inverted-block]
     (p/render-tmpl inverted-block w (p/ctx-push ctx this))))

  Object
  (render
    ([this w _ _]
     ;; todo: escape
     (p/append w (str this)))
    ([this w ctx attrs block _]
     (p/render-tmpl block w (p/ctx-push ctx this))))

  String
  (render
    ([this w ctx attrs]
     ;; todo: escape
     (p/append w this))
    ([this w ctx attrs block inverted-block]
     (if-not (str/blank? this)
       (p/render-tmpl block w (p/ctx-push ctx this))
       (p/render-tmpl inverted-block w ctx))))

  Boolean
  (render
    ([this w ctx attrs]
     (p/append w (str this)))
    ([this w ctx attrs block inverted-block]
     (if this
       (p/render-tmpl block w (p/ctx-push ctx this))
       (p/render-tmpl inverted-block w ctx))))

  clojure.lang.Sequential
  (render
    ([this w ctx attrs]
     ;; todo: escape
     (p/append w (str this)))
    ([this w ctx attrs block inverted-block]
     (if (seq this)
       (let [separator (get attrs :separator space)
             separator #(p/render-tmpl separator w ctx)]
         (doseq [tmpl (interpose separator
                                 (for [item this]
                                   #(p/render-tmpl block w (p/ctx-push ctx item))))]
            (tmpl)))
       (p/render-tmpl inverted-block w ctx))))

  clojure.lang.IPersistentMap
  (render
    ([this w ctx attrs]
     ;; todo: escape
     (p/append w (str this)))
    ([this w ctx attrs block inverted-block]
     (if (seq this)
       (p/render-tmpl block w (p/ctx-push ctx this))
       (p/render-tmpl inverted-block w ctx))))

  clojure.lang.Fn
  (render
    ([this w ctx attrs]
     (p/render (this ctx attrs) w ctx attrs))
    ([this w ctx attrs block inverted-block]
     (p/render (this ctx attrs) w ctx attrs block inverted-block))))

(extend-protocol p/Template
  String
  (render-tmpl [this w _]
    ;; todo: escape
    (p/append w this)))
