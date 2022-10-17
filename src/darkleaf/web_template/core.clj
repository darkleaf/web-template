(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:require
   [clojure.string :as str]
   [clojure.pprint :as pp])
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
  [tag ^:attrs (:attrs) & body]

  (:key ^:attrs (:attrs))
  (:key (:attrs) nil nil)
  (:key (:attrs) block nil)
  (:key (:attrs) block inverted-block)

  ;; так как-то получше выглядит
  (:key {. :attrs})

  [div {class (:class)}]
  [div {(:attr) true}]
  [div {. :attrs}]
  [div {data {foo "43"}}]

  [div {class (:class "default")}]

  [.foo#bar {data {(:segment) {foo  (:x)
                               (:y) "bar"
                               "xyz" "42"}}}]

  (:panel-component {:title [.title (:title)]}
                    "content"
                    "empty"))

;; tmpl :: w data -> ()


(defprotocol Section
  (write [this writer ctx attrs block-tmpl inverted-block-tmpl]))

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


;; todo: escape
;; в теории, можно в какой-нибудь кривой symbol положить что-то не хорошее
(defn- default-write [this ^Writer w ctx attrs]
  (cond
    ;; todo? clojure.pprint/cl-format
    ;; не работает с clojure.lang.Ratio
    (:format attrs) (.append w (format (:format attrs) this))
    (:pretty attrs) (pp/pprint this w)
    true            (.append w (str this))))

(defn- dynamic-node [node]
  (when (list? node)
    (let [key            (nth node 0 nil)
          attrs?         (map? (nth node 1 nil))
          attrs          (if attrs? (nth node 1 nil))
          block          (-> node (nth (if attrs? 2 1) nil) compile)
          inverted-block (-> node (nth (if attrs? 3 2) nil) compile)
          write'         (if (= (if attrs? 2 1)
                                (count node))
                           default-write
                           #(write %1 %2 %3 %4 block inverted-block))]
      (fn [w ctx]
        (let [value (get ctx key)
              ctx   (ctx-push ctx value)]
          (write' value w ctx attrs))))))

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

(extend-protocol Section
  nil
  (write[_ w ctx attrs _ inverted-block-tmpl]
    (inverted-block-tmpl w ctx))

  String
  (write [this ^Writer w ctx attrs block-tmpl inverted-block-tmpl]
    (if-not (str/blank? this)
      (block-tmpl w ctx)
      (inverted-block-tmpl w ctx)))

  Boolean
  (write [this w ctx attrs block-tmpl inverted-block-tmpl]
    (if this
      (block-tmpl w ctx)
      (inverted-block-tmpl w ctx)))

  clojure.lang.Sequential
  (write [this ^Writer w ctx attrs block-tmpl inverted-block-tmpl]
    (if (seq this)
      (let [separator      (str (get attrs :separator " "))
            separator-tmpl #(.append w separator)]
        (doseq [tmpl (interpose separator-tmpl
                                (for [item this
                                      :let [ctx (ctx-push ctx item)]]
                                  #(block-tmpl w ctx)))]
           (tmpl)))
      (inverted-block-tmpl w ctx)))

  clojure.lang.IPersistentMap
  (write [this w ctx attrs block-tmpl inverted-block-tmpl]
    (if (seq this)
      (block-tmpl w ctx)
      (inverted-block-tmpl w ctx))))
