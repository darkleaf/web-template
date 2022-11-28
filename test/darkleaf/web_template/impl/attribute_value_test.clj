(ns darkleaf.web-template.impl.attribute-value-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(defmacro test-tmpl
  {:style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string (wt/compile dsl#)
                                                       data#))
       ~@body)))


(t/deftest nil-test
  (test-tmpl
    [div {id nil}]
    nil
    "<div></div>"

    [div#a {id nil}]
    nil
    "<div></div>"))

(t/deftest false-test
  (test-tmpl
    [div {id false}]
    nil
    "<div></div>"

    [div#a {id false}]
    nil
    "<div></div>"))

(t/deftest true-test
  (test-tmpl
    ;; todo xml, html5
    [div {foo true}]
    nil
    "<div foo=\"true\"></div>"))

(t/deftest string-test
  (test-tmpl
    [div {id "a"}]
    nil
    "<div id=\"a\"></div>"

    [div#a {id "b"}]
    nil
    "<div id=\"b\"></div>"))

(t/deftest object-test
  (let [value (reify Object
                (toString [_]
                  "stub"))]
    (test-tmpl
      [div {id ~value}]
      nil
      "<div id=\"stub\"></div>"

      [div#a {id ~value}]
      nil
      "<div id=\"stub\"></div>")))

(t/deftest ident-test
  (test-tmpl
    [div {id :a}]
    nil
    "<div id=\"a\"></div>"

    [div {id a}]
    nil
    "<div id=\"a\"></div>"

    [div#a {id :b}]
    nil
    "<div id=\"b\"></div>"

    [div#a {id b}]
    nil
    "<div id=\"b\"></div>"))

(t/deftest map-test
  (test-tmpl
    [div {class {a true b true}}]
    nil
    "<div class=\"a b\"></div>"

    [div {class {a true b false}}]
    nil
    "<div class=\"a\"></div>"

    [div {class {a true b nil}}]
    nil
    "<div class=\"a\"></div>"

    [div.a {class {a false b true}}]
    nil
    "<div class=\"a b\"></div>"))

(t/deftest seqable-test
  (test-tmpl
    [div.a {class [b c]}]
    nil
    "<div class=\"a b c\"></div>"))

(t/deftest fn-test
  (test-tmpl
    [div.a {class ~(fn [ctx] (:class ctx))}]
    {:class [:b :c]}
    "<div class=\"a b c\"></div>"))












(t/deftest attrs-test
  (test-tmpl
    [div]
    nil
    "<div></div>"

    [div#a.b.c]
    nil
    "<div id=\"a\" class=\"b c\"></div>"


    [div#a {id b}]
    nil
    "<div id=\"b\"></div>"

    [div#a {:id :b}]
    nil
    "<div id=\"b\"></div>"

    [div#a {"id" "b"}]
    nil
    "<div id=\"b\"></div>"


    [.a {class #{b}}]
    nil
    "<div class=\"a b\"></div>"

    [.a {class #{:b}}]
    nil
    "<div class=\"a b\"></div>"

    [.a {class #{"b"}}]
    nil
    "<div class=\"a b\"></div>"


    [.a {class #{b}, ... (:attrs)}]
    {:attrs {:class "c"}}
    "<div class=\"c\"></div>"

    [.a {class #{b}
         ...   (:attrs)}]
    {:attrs {:class ["c"]}}
    "<div class=\"a b c\"></div>"

    [div {... (:attrs)}]
    {:attrs {:class {:a true
                     :b false}}}
    "<div class=\"a\"></div>"


    [.a {class nil}]
    nil
    "<div></div>"

    [.a {class #{b}
         ...   (:attrs)}]
    {:attrs {:class nil}}
    "<div></div>"

    [.a {class false}]
    nil
    "<div></div>"

    [.a {class #{b}
         ...   (:attrs)}]
    {:attrs {:class false}}
    "<div></div>"


    ;; todo
    [div {attr true}]
    nil
    "<div attr=\"true\"></div>")



  (comment
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
