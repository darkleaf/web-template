(ns darkleaf.web-template.impl.element-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]
   [darkleaf.web-template.protocols :as wtp]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

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
    nil
    nil
    ""))

(t/deftest literal-string-test
  (test-tmpl
    "a"
    nil
    "a"

    "<div />"
    nil
    "<div />"))

(t/deftest vector-test
  (test-tmpl
    [div]
    nil
    "<div></div>"

    [div "a"]
    nil
    "<div>a</div>"

    [div "a" " " "b"]
    nil
    "<div>a b</div>"

    [div.foo]
    nil
    "<div class=\"foo\"></div>"

    [div {class "foo"}]
    nil
    "<div class=\"foo\"></div>"

    [div [div]]
    nil
    "<div><div></div></div>"

    [:div]
    nil
    "<div></div>"

    ["div"]
    nil
    "<div></div>"))

(t/deftest <>-test
  (test-tmpl
    [<> "foo" " " "bar"]
    nil
    "foo bar"))

(t/deftest list-test
  (let [value (reify wtp/Value
                (write-value [this w ctx]
                  (t/is (= {'this this} ctx))
                  (w/append-raw w "stub/3"))
                (write-value [this w ctx block inverted-block]
                  (t/is (= {'this this} ctx))
                  (w/append-raw w "stub/5")
                  (w/append-raw w " ") (wtp/render block w ctx)
                  (w/append-raw w " ") (wtp/render inverted-block w ctx)))]
    (test-tmpl
      (this)
      value
      "stub/3"

      (this "present")
      value
      "stub/5 present "

      (this "present" "blank")
      value
      "stub/5 present blank")))

(t/deftest renderable-test
  (let [renderable (reify wtp/Renderable
                     (render [this w ctx]
                       (t/is (= {'this ::data} ctx))
                       (w/append-raw w "stub")))]
    (test-tmpl
      ~renderable
      ::data
      "stub")))
