(ns darkleaf.web-template.protocols
  (:import
   (java.io Writer)))

(set! *warn-on-reflection* true)

(defprotocol Template
  (render-tmpl [this writer ctx]))

(defprotocol Component
  (render [this writer ctx])
  (render-present [this writer ctx block])
  (render-blank [this writer ctx block]))

(defn ctx-push [ctx v]
  (merge ctx
         (if (map? v) v)
         {'. v}))

(defn append [^Writer w ^String str]
  (.append w str))
