;; *********************************************************************
;; * Copyright (c) 2012 Brandon Bloom
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

;; https://github.com/brandonbloom/backtick/blob/master/src/backtick.clj

(ns darkleaf.web-template.internal.backtick
  (:refer-clojure :exclude [var?]))

(defn- unquote? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote)))

(defn- unquote-splicing? [form]
  (and (seq? form) (= (first form) 'clojure.core/unquote-splicing)))

(defn- var? [form]
  (and (seq? form) (= (first form) 'var)))

(defn template-fn [form]
  (cond
    (symbol? form) `'~form
    (unquote? form) (second form)
    (unquote-splicing? form) (throw (ex-info "Splice not in list" {:form form}))
    (record? form) `'~form
    (var? form) form
    (coll? form)
    (let [xs (if (map? form) (apply concat form) form)
          parts (for [x xs]
                  (if (unquote-splicing? x)
                    (second x)
                    [(template-fn x)]))
          cat (doall `(concat ~@parts))]
      (cond
        (vector? form) `(vec ~cat)
        (map? form) `(apply hash-map ~cat)
        (set? form) `(set ~cat)
        (seq? form) `(apply list ~cat)
        :else (throw (ex-info "Unknown collection type" {:form form}))))
    :else `'~form))

(defmacro template [form]
  (template-fn form))
