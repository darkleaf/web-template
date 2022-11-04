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

  (:tags)
  (:tags str)

  (:tags (.))
  (:tags blank? "to tags")
  (:tags (format "") .)

  (defn format [obj key & args])

  (:price (i18n)) (i18n 42 :price)

  (:price (i18n :foo/price)) (i18n 42 :price :foo/price)
  (:price (i18n :foo/price (:currency))) (i18n 42 :price :foo/price :rub)

  (compile [div])
  (compile* '[div]))

(declare compile*)

(defmacro template
  {:style/indent :defn :private true}
  [[writer ctx] & body]
  `(reify
     p/Template
     (render-tmpl [this# ~writer ~ctx]
       ~@body)
     p/Component
     (render [this# w# ctx#]
       (.render-tmpl this# w# ctx#))
     (render-present [this# w# ctx# block#]
       (.render this# w# (assoc ctx# :block block#)))
     (render-blank [this# w# ctx# block#])))

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

    (-> body second meta prn)

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

;; todo: 3 отдельных обработчика
(defn- dynamic-node [node]
  (when (list? node)
    (let [key    (nth node 0 nil)
          block  (-> node second compile*)
          render (cond
                   (= 1 (count node))    p/render
                   (-> node meta :blank) #(p/render-blank %1 %2 %3 block)
                   :else                 #(p/render-present %1 %2 %3 block))]
      (template [w ctx]
        (let [component (get ctx key)]
          (render component w ctx))))))

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

(defn render-to-string
  ([template data] (render-to-string template nil data))
  ([template ctx data]
   (let [sw  (StringWriter.)]
     (p/render-tmpl template sw (p/ctx-push ctx data))
     (.toString sw))))

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
      (p/render-tmpl block w ctx))))
