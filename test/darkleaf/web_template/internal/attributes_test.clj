(ns darkleaf.web-template.internal.attributes-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]
   [darkleaf.web-template.protocols :as p]))

(t/deftest merge-attrs-test
  (let [replace (reify p/AttributeValue
                  (update-attribute-value [_ _ value]
                     "replaced"))
        append  (reify p/AttributeValue
                  (update-attribute-value [_ _ value]
                    (str value " " "appended")))
        delete  (reify p/AttributeValue
                  (update-attribute-value [_ _ value]
                    nil))
        ctx     (reify p/AttributeValue
                  (update-attribute-value [_ ctx value]
                    (:foo ctx)))]
    (t/are [literal attrs ctx result]
        (t/is (= result (merge-attrs literal attrs ctx)))
      nil nil nil
      nil

      {"id" "a"} nil nil
      {"id" "a"}


      nil {"id" replace} nil
      {"id" "replaced"}

      nil {:id replace} nil
      {"id" "replaced"}

      nil {'id replace} nil
      {"id" "replaced"}

      {"id" "a"} {:id replace} nil
      {"id" "replaced"}


      {"id" "a"} {:id append} nil
      {"id" "a appended"}

      nil {:id delete} nil
      nil

      {"id" "a" "class" "b"} {:id delete} nil
      {"class" "b"}

      nil {:id ctx} {:foo "a"}
      {"id" "a"})))

(t/deftest resolve-test
  (let [stub (reify p/AttributeValue
               (update-attribute-value [_ _ _]
                 "stub"))]
    (t/are [attrs ctx result]
        (t/is (= result (merge-attrs nil attrs ctx)))
      '{foo (:foo)} {:foo stub}
      {"foo" "stub"})))

(t/deftest spread-test
  (let [stub (reify p/AttributeValue
               (update-attribute-value [_ _ value]
                 (str value " " "stub")))]
    (t/are [attrs ctx result]
        (t/is (= result (merge-attrs nil attrs ctx)))
      '{... (:attrs)} {:attrs {:foo stub}}
      {"foo" " stub"}

      {:foo stub, '...  '(:attrs)} {:attrs {:foo stub}}
      {"foo" " stub stub"}

      {'foo stub, '...  '(:attrs)} {:attrs {:foo stub}}
      {"foo" " stub stub"})))

(t/deftest namespace-test
  (let [stub (reify p/AttributeValue
               (update-attribute-value [_ _ _]
                 "stub"))]
    (t/are [attrs result]
        (t/is (= result (merge-attrs nil attrs nil)))
      {:foo/bar stub}
      {"foo:bar" "stub"}

      {'foo/bar stub}
      {"foo:bar" "stub"})))
