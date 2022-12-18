(ns darkleaf.web-template.impl.renderable-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(t/deftest nil-test
  (let [renderable nil]
    (t/is (= "" (wt/render-to-string renderable nil)))))

(t/deftest object-test
  (let [renderable (reify Object
                     (toString [_]
                       "obj"))]
    (t/is (= "obj" (wt/render-to-string renderable nil)))))

(t/deftest fn-test
  (let [renderable (fn [ctx]
                    (t/is (= ::data ctx))
                    "stub")
        data  ::data]
    (t/is (= "stub" (wt/render-to-string renderable data)))))

(def part "part")

(t/deftest var-test
  (do
    (t/is (= "part"
             (wt/render-to-string #'part nil))))
  (with-redefs [part "redefined"]
    (t/is (= "redefined"
             (wt/render-to-string #'part nil)))))
