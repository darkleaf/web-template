(ns darkleaf.web-template.internal.attributes-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.internal.attributes :refer [merge-attrs]]))

(t/deftest merge-attrs-test
  (t/are [literal attrs ctx result]
      (t/is (= result (merge-attrs literal (quote attrs) ctx)))
    nil nil nil
    nil

    {"id" "a", "class" "b c"} nil nil
    {"id" "a", "class" "b c"}


    {"id" "a"} {id b} nil
    {"id" "b"}

    {"id" "a"} {:id :b} nil
    {"id" "b"}

    {"id" "a"} {"id" "b"} nil
    {"id" "b"}


    {"class" "a"} {class #{b}} nil
    {"class" "a b"}

    {"class" "a"} {:class #{:b}} nil
    {"class" "a b"}

    {"class" "a"} {"class" #{"b"}} nil
    {"class" "a b"}


    {"class" "a"} {class #{b} ... (:attrs)} {:attrs {:class "c"}}
    {"class" "c"}

    {"class" "a"} {class #{b} ... (:attrs)} {:attrs {:class ["c"]}}
    {"class" "a b c"}

    nil {... (:attrs)} {:attrs {:class {:a true, :b false}}}
    {"class" "a"}


    {"class" "a"} {class nil} nil
    {"class" "a"}

    {"class" "a"} {class #{b} ... (:attrs)} {:attrs {:class nil}}
    {"class" "a b"}


    {"class" "a"} {class false} nil
    {}

    {"class" "a"} {class #{b} ... (:attrs)} {:attrs {:class false}}
    {}


    nil {attr true} nil
    {"attr" true}

    nil {... (:attrs)} {:attrs {:attr true}}
    {"attr" true}


    nil {attr 3/4} nil
    {"attr" "3/4"}


    nil {class (:class)} {:class "a"}
    {"class" "a"}

    {"class" "a"} {class (:class)} {:class (list "b")}
    {"class" "a b"}


    nil {(:attr) 42} {:attr "my-attr"}
    {"my-attr" "42"}

    nil {class {foo (:foo)}} {:foo true}
    {"class" "foo"}

    ;; todo
    nil {class {foo (:foo)}} {:foo false}
    {"class" ""}))
