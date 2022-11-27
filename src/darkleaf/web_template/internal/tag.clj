;; *********************************************************************
;; * Copyright (c) 2013-2020 r0man
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

;; https://github.com/r0man/sablono/blob/master/src/sablono/normalize.cljc

(ns darkleaf.web-template.internal.tag
  (:require
   [clojure.string :as str]))

(defn- strip-css
  "Strip the # and . characters from the beginning of `s`."
  [s]
  (str/replace s #"^[.#]" ""))

(defn- add-ns [ns s]
  (if ns
    (str ns ":" s)
    s))

(defn- parse-ident-tag [tag]
  (let [add-ns  (partial add-ns (namespace tag))
        matches (re-seq #"[#.]?[^#.]+" (name tag))
        [tag-name names]
        (cond (empty? matches)
              (throw (ex-info (str "Can't match CSS tag: " tag) {:tag tag}))

              (#{\# \.} (ffirst matches)) ;; shorthand for div
              ["div" matches]

              :default
              [(first matches) (rest matches)])
        tag-name         (add-ns tag-name)
        {id \# class \.} (group-by first names)
        id               (some->> id first strip-css)
        class            (some->> class (map strip-css) seq (str/join " "))
        attrs            (cond-> nil
                           id    (assoc (add-ns "id") id)
                           class (assoc (add-ns "class") class))]
    [tag-name attrs]))

(defn parse-tag [tag]
  (if (string? tag)
    [tag nil]
    (parse-ident-tag tag)))
