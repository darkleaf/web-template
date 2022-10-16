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
  [tag attributes & body])

;; tmpl :: w data -> ()

(defprotocol Section
  ;; todo: attrs (:foo {:class "foo"} (.) "not found"
  (write
    [this writer ctx]
    [this writer ctx block-tmpl inverted-block-tmpl]))

(defn- nil-tmpl [_ _])

(defn- nil-node [node]
  (when (nil? node)
    nil-tmpl))

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
        (doseq [item body]
          (item w ctx)
          (.append w " "))))))

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

        (doseq [item body]
          (item w ctx))

        (.append w "</")
        (.append w tag)
        (.append w ">")))))

(defn- dynamic-node [[key block inverted-block :as node]]
  (when (list? node)
    (let [write' (if (= 1 (count node))
                   write
                   #(write %1 %2 %3 (compile block) (compile inverted-block)))]
      (fn [w ctx]
        (let [value (get ctx key)
              ctx   (ctx-push ctx value)]
          (write' value w ctx))))))

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
  (write
    ([this ^Writer w ctx]
     (.append w "nil"))
    ([_ w ctx _ inverted-block-tmpl]
     (inverted-block-tmpl w ctx)))

  ;; todo: escape
  String
  (write
    ([this ^Writer w ctx]
     (.append w this))
    ([this ^Writer w ctx block-tmpl inverted-block-tmpl]
     (if-not (str/blank? this)
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx))))

  Boolean
  (write
    ([this ^Writer w ctx]
     (.append w (str this)))
    ([this w ctx block-tmpl inverted-block-tmpl]
     (if this
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx))))

  clojure.lang.Sequential
  (write
    ([this ^Writer w ctx]
     (pp/pprint this w))
    ([this ^Writer w ctx block-tmpl inverted-block-tmpl]
     (if (seq this)
       (doseq [item this
               :let [ctx (ctx-push ctx item)]]
         (block-tmpl w ctx)
         (.append w " "))
       (inverted-block-tmpl w ctx))))

  clojure.lang.IPersistentMap
  (write
    ([this w ctx]
     (pp/pprint this w))
    ([this w ctx block-tmpl inverted-block-tmpl]
     (if (seq this)
       (block-tmpl w ctx)
       (inverted-block-tmpl w ctx)))))

  ;; todo: nubmers: long, double {:format "%..."}

  ;; Object
  ;; (write
  ;;   ;; todo: throw
  ;;   ([this w ctx]
  ;;    (pp/pprint this w))))
