;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.writer
  (:import
   (java.io Writer StringWriter)))

(set! *warn-on-reflection* true)

;; TODO: for cljs use https://google.github.io/closure-library/api/goog.string.StringBuffer.html

(defn- ^String escape [^String s]
  (.. s
      (replace "&"  "&amp;")
      (replace "<"  "&lt;")
      (replace ">"  "&gt;")
      (replace "\"" "&quot;")
      (replace "'"  "&apos;")))

(defn append-raw [^Writer w ^String str]
  (.append w str))

(defn append [^Writer w ^String str]
  (->> str
       escape
       (.append w)))

(defn write-to-string [callback]
  (let [sw (StringWriter.)]
    (callback sw)
    (.toString sw)))
