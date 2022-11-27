(ns darkleaf.web-template.impl.renderable-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(t/deftest nil-test
  (let [value nil
        tmpl  (wt/compile ~value)]
    (t/is (= "" (wt/render-to-string tmpl nil)))))

(t/deftest object-test
  (let [value (reify Object
                (toString [_]
                  "obj"))
        tmpl (wt/compile ~value)]
    (t/is (= "obj" (wt/render-to-string tmpl nil)))))

(t/deftest fn-test
  (let [value (fn [ctx]
                (t/is (= {'this ::data} ctx))
                "stub")
        tmpl  (wt/compile ~value)
        data  ::data]
    (t/is (= "stub" (wt/render-to-string tmpl data)))))

(def part "part")

(t/deftest var-test
  (let [tmpl (wt/compile #'part)]
    (do
      (t/is (= "part"
               (wt/render-to-string tmpl nil))))
    (with-redefs [part "redefined"]
      (t/is (= "redefined"
               (wt/render-to-string tmpl nil))))))
