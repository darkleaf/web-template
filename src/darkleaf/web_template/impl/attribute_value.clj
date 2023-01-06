;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.impl.attribute-value
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]))

(defn attribute-value-seqable [this]
  (->> this
       (filter some?)
       (map p/attribute-value)
       (str/join " ")))

(defn attribute-value-map [this]
  (let [this (->> this
                  (filter val)
                  (map key))]
    (attribute-value-seqable this)))

(defn attribute-value-ident [this]
  ;; todo? foo/bar -> foo--bar, stimulus
  (name this))

(defn attribute-value-default [this]
  (str this))

(extend-protocol p/AttributeValue
  nil
  (attribute-value [_]
    nil)

  Object
  (attribute-value [this]
    (cond
      (ident? this)   (attribute-value-ident   this)
      (map? this)     (attribute-value-map     this)
      (seqable? this) (attribute-value-seqable this)
      :default        (attribute-value-default this)))

  Boolean
  (attribute-value [this]
    (when this
      true))

  String
  (attribute-value [this]
    this))
