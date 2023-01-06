;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.internal.attributes-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]
   [darkleaf.web-template.protocols :as p]))

(t/deftest merge-attrs-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_]
                 "stub"))]
    (t/are [literal attrs ctx result]
        (t/is (= result (merge-attrs literal attrs ctx)))
      nil nil nil
      {}

      {"id" "a"} nil nil
      {"id" "a"}


      nil {"id" stub} nil
      {"id" "stub"}

      nil {:id stub} nil
      {"id" "stub"}

      nil {'id stub} nil
      {"id" "stub"}


      {"attr" "a"} {:attr stub} nil
      {"attr" "a stub"})))


(t/deftest resolve-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_]
                 "stub"))]
    (t/are [attrs ctx result]
        (t/is (= result (merge-attrs nil attrs ctx)))
      '{foo (:foo)} {:foo stub}
      {"foo" "stub"})))

(t/deftest spread-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_]
                 "stub"))]
    (t/are [attrs ctx result]
        (t/is (= result (merge-attrs nil attrs ctx)))
      '{... (:attrs)} {:attrs {:foo stub}}
      {"foo" "stub"}

      {:foo stub, '...  '(:attrs)} {:attrs {:foo stub}}
      {"foo" "stub stub"}

      {'foo stub, '...  '(:attrs)} {:attrs {:foo stub}}
      {"foo" "stub stub"})))

(t/deftest namespace-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_]
                 "stub"))]
    (t/are [attrs result]
        (t/is (= result (merge-attrs nil attrs nil)))
      {:foo/bar stub}
      {"foo:bar" "stub"}

      {'foo/bar stub}
      {"foo:bar" "stub"})))
