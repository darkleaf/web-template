(ns darkleaf.web-template.core
  (:refer-clojure :exclude [compile])
  (:import
   (java.io Writer StringWriter)))

(set! *warn-on-reflection* true)

(declare compile)

(comment
  [tag & body]
  [tag attributes & body])

(defprotocol DynamicNode
  (to-writer [this args]))

(defn- string-node [node]
  (when (string? node)
    (fn [data]
      (fn [^Writer w]
        (.append w ^String node)))))

(defn- static-tag--no-attrs--body [[tag & body :as node]]
  (when (and (vector? node)
             (ident? tag)
             (not (-> body first map?)))
    (let [tag  (name tag)
          body (map compile body)]
      (fn [data]
        (fn [^Writer w]
          (.append w "<")
          (.append w tag)
          (.append w ">")

          (doseq [item body]
            ((item data) w))

          (.append w "</")
          (.append w tag)
          (.append w ">"))))))

(defn- dynamic-node [[key & args :as node]]
  (when (list? node)
    (fn [data]
      (let [data (get data key)]
        (to-writer data args)))))

(defmacro chain-handlers
  {:private true :style/indent :defn}
  [node-binding & handlers]
  `(or ~@(for [h handlers]
           `(~h ~node-binding))))

(defn compile [node]
  (chain-handlers node
    string-node
    dynamic-node
    static-tag--no-attrs--body))



(defn render-to-string [template data]
  (let [sw (StringWriter.)]
    ((template data) sw)
    (.toString sw)))

(comment
  '[div {}
    ""
    [div {}]])


(extend-protocol DynamicNode
  String
  (to-writer [this _]
    (fn [^Writer w]
      (.append w this)))

  ;; todo
  clojure.lang.PersistentVector
  ;; todo: empty
  (to-writer [this [each-tmpl #_empty-tmpl]]
    (let [each-tmpl (compile each-tmpl)
          writers   (map each-tmpl this)]
      (fn [^Writer w]
        (doseq [writer writers]
          (writer w))))))
