(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [darkleaf.web-template.protocols :as p])
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

;; tmpl :: w data -> ()


;; tmpl должен реализовывать Section / Component
;; и block должен быть в контексте
;; нужно, чтобы layout сделать


(defn- separator-tmpl [^Writer w _]
  (.append w " "))

(defn- nil-node [node]
  (when (nil? node)
    (fn [_ _])))

(declare dynamic-node)

(defn- dot-node [node]
  (when (= '. node)
    (dynamic-node '(.))))

(defn- string-node [node]
  (when (string? node)
    (fn [^Writer w ctx]
      (.append w ^String node))))

(defn- <>-node [[tag & body :as node]]
  (when (and (vector? node)
             (= '<> tag))
    (let [body (map compile body)]
      (fn [^Writer w ctx]
        (doseq [item (interpose separator-tmpl body)]
          (item w ctx))))))

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

        (doseq [item (interpose separator-tmpl body)]
          (item w ctx))

        (.append w "</")
        (.append w tag)
        (.append w ">")))))

(defn- dynamic-node [node]
  (when (list? node)
    (let [key            (nth node 0 nil)
          attrs?         (map? (nth node 1 nil))
          attrs          (if attrs? (nth node 1 nil))
          block          (-> node (nth (if attrs? 2 1) nil) compile)
          inverted-block (-> node (nth (if attrs? 3 2) nil) compile)
          render'         (if (= (if attrs? 2 1)
                                 (count node))
                            p/render
                            #(p/render %1 %2 %3 %4 block inverted-block))]
      (fn [w ctx]
        (let [value (get ctx key)
              ctx   (ctx-push ctx value)]
          (render' value w ctx attrs))))))

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
    static-tag--no-attrs--body))

(defn render-to-string [template data]
  (let [sw  (StringWriter.)
        ctx (ctx-push nil data)]
    (template sw ctx)
    (.toString sw)))

(extend-protocol p/Component
  nil
  (render
    ([this _ _ _])
    ([_ w ctx attrs _ inverted-block-tmpl]
     (inverted-block-tmpl w ctx)))

  String
  (render
    ([this ^Writer w ctx attrs]
     (.append w this))
    ([this ^Writer w ctx attrs block-tmpl inverted-block-tmpl]
     (if-not (str/blank? this)
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx))))

  Boolean
  (render
    ([this w ctx attrs]
     (.append ^Writer w (str this)))
    ([this w ctx attrs block-tmpl inverted-block-tmpl]
     (if this
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx))))

  clojure.lang.Sequential
  (render
    ([this w ctx attrs]
     ;; todo: escape
     (.append ^Writer w (str this)))
    ([this ^Writer w ctx attrs block-tmpl inverted-block-tmpl]
     (if (seq this)
       (let [separator      (str (get attrs :separator " "))
             separator-tmpl #(.append w separator)]
         (doseq [tmpl (interpose separator-tmpl
                                 (for [item this
                                       :let [ctx (ctx-push ctx item)]]
                                   #(block-tmpl w ctx)))]
            (tmpl)))
       (inverted-block-tmpl w ctx))))

  clojure.lang.IPersistentMap
  (render
    ([this w ctx attrs]
     ;; todo: escape
     (.append ^Writer w (str this)))
    ([this w ctx attrs block-tmpl inverted-block-tmpl]
     (if (seq this)
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx)))))
