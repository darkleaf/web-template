;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.impl.renderable-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(t/deftest nil-test
  (let [renderable nil]
    (t/is (= "" (wt/render-to-string renderable)))))

(t/deftest object-test
  (let [renderable (reify Object
                     (toString [_]
                       "obj"))]
    (t/is (= "obj" (wt/render-to-string renderable)))))

#_#_
(def part "part")
(t/deftest var-test
  (do
    (t/is (= "part"
             (wt/render-to-string #'part))))
  (with-redefs [part "redefined"]
    (t/is (= "redefined"
             (wt/render-to-string #'part)))))

(t/deftest seqable-test
  (let [renderable (interpose ", " [1 2 3])]
    (t/is (= "1, 2, 3"
             (wt/render-to-string renderable)))))
