(ns darkleaf.web-template.components
  (:refer-clojure :exclude [format])
  (:require
   [clojure.core :as c]
   [darkleaf.web-template.protocols :as p]))

(set! *warn-on-reflection* true)

(defn- format* [ctx attrs]
  (let [fmt (attrs :fmt "")
        dot (ctx '.)
        a1  (attrs 1 dot)
        a2  (attrs 2)
        a3  (attrs 3)
        a4  (attrs 4)]
    (c/format fmt a1 a2 a3 a4)))

(def format
  (reify p/Component
    (render [_  w ctx attrs]
      ;; todo: escape
      (p/append w (format* ctx attrs)))
    (render [_ w ctx attrs block _]
      (p/render-tmpl block w (p/ctx-push ctx (format* ctx attrs))))))
