;; *********************************************************************
;; * Copyright (c) 2012 Brandon Bloom
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

;; https://github.com/brandonbloom/backtick/blob/master/test/backtick_test.clj

(ns darkleaf.web-template.internal.backtick-test
  (:require
   [darkleaf.web-template.internal.backtick :refer [template]]
   [clojure.test :as t]))

(t/deftest template-test
  (t/testing "Primitives, collections, unquote, and splice; symbols qualified"
    (let [n 5 v [:a :b]]
      (t/is (=           `(5 nil () true a/b ~n [p/q ~@v r/s] {:x #{"s"}})
                (template (5 nil () true a/b ~n [p/q ~@v r/s] {:x #{"s"}}))))))

  (t/testing "Multiple splices"
    (let [v [:a :b] a 5]
      (t/is (=           `(~a ~@v ~@v ~a)
               (template  (~a ~@v ~@v ~a)))))))

(defrecord R [x])

(t/deftest record-test
  (t/testing "Record types are preserved"
    (t/is (= (R. 1) (template #darkleaf.web_template.internal.backtick_test.R{:x 1})))))
