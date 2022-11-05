(ns darkleaf.web-template.protocols
  (:refer-clojure :exclude [compile])
  (:import
   (java.io Writer)))

(set! *warn-on-reflection* true)

(defprotocol Element
  (compile [this]))

(defprotocol Renderable
  (render [this writer ctx]))

(defprotocol Value
  (write
    [this writer ctx]
    [this writer ctx block inverted-block]))

(defn ctx-push [ctx v]
  (merge ctx
         (if (map? v) v)
         {'this v}))

(defn append-raw [^Writer w ^String str]
  (.append w str))

(defn append [^Writer w ^String str]
  (.append w str))
