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

(def part "part")

;; TODO: Fix this test
;; #object[RangeError RangeError: Maximum call stack size exceeded]
(t/deftest var-test
  (do
    (t/is (= "part"
             (wt/render-to-string #'part))))
  (with-redefs [part "redefined"]
    (t/is (= "redefined"
             (wt/render-to-string #'part)))))

;; TODO: Fix this test
;; #object[RangeError RangeError: Maximum call stack size exceeded]
(t/deftest seqable-test
  (let [renderable (interpose ", " [1 2 3])]
    (t/is (= "1, 2, 3"
             (wt/render-to-string renderable)))))
