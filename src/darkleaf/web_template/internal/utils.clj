(ns darkleaf.web-template.internal.utils
  (:refer-clojure :exclude [namespace])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]))

(defn join-some [sep coll]
  (->> coll
       (filter some?)
       (str/join sep)))

(defn namespace [x]
  (if (ident? x)
    (c/namespace x)))
