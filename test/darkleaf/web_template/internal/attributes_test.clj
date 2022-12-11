(ns darkleaf.web-template.internal.attributes-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]
   [darkleaf.web-template.protocols :as p]))

(t/deftest merge-attrs-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_ _]
                 "stub"))
        ctx     (reify p/AttributeValue
                  (attribute-value [_ ctx]
                    (:foo ctx)))]
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
      {"attr" "a stub"}


      nil {:id ctx} {:foo "a"}
      {"id" "a"})))

(t/deftest resolve-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_ _]
                 "stub"))]
    (t/are [attrs ctx result]
        (t/is (= result (merge-attrs nil attrs ctx)))
      '{foo (:foo)} {:foo stub}
      {"foo" "stub"})))

(t/deftest spread-test
  (let [stub (reify p/AttributeValue
               (attribute-value [_ _]
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
               (attribute-value [_ _]
                 "stub"))]
    (t/are [attrs result]
        (t/is (= result (merge-attrs nil attrs nil)))
      {:foo/bar stub}
      {"foo:bar" "stub"}

      {'foo/bar stub}
      {"foo:bar" "stub"})))
