(ns darkleaf.web-template.impl.element-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]
   [darkleaf.web-template.protocols :as wtp]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defmacro test-tmpl
  {:style/indent :defn}
  [mode & body]
  (let [[mode body] (if (symbol? mode)
                      [mode body]
                      [wt/html5-mode (cons mode body)])]
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string (wt/compile (quote dsl#) ~mode)
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
  (let [value (reify
                wtp/Renderable
                (render [this w ctx]
                  (t/is (= this ctx))
                  (w/append-raw w "stub/3"))
                wtp/Container
                (container->renderable [this block inverted-block]
                  (reify wtp/Renderable
                    (render [_ w ctx]
                      (t/is (= this ctx))
                      (w/append-raw w "stub/5")
                      (w/append-raw w " ") (wtp/render block w ctx)
                      (w/append-raw w " ") (wtp/render inverted-block w ctx)))))]
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

(t/deftest void-tags-test
  (test-tmpl
    [area]
    nil
    "<area>"

    [base]
    nil
    "<base>"

    [br]
    nil
    "<br>"

    [col]
    nil
    "<col>"

    [command]
    nil
    "<command>"

    [embed]
    nil
    "<embed>"

    [hr]
    nil
    "<hr>"

    [img]
    nil
    "<img>"

    [input]
    nil
    "<input>"

    [keygen]
    nil
    "<keygen>"

    [link]
    nil
    "<link>"

    [meta]
    nil
    "<meta>"

    [param]
    nil
    "<param>"

    [source]
    nil
    "<source>"

    [track]
    nil
    "<track>"

    [wbr]
    nil
    "<wbr>"))

(t/deftest empty-attributes-test
  (test-tmpl
    [input {disabled true}]
    nil
    "<input disabled>"))

(t/deftest xml-attributes-test
  (test-tmpl wt/xml-mode
    [input {disabled true}]
    nil
    "<input disabled=\"disabled\"></input>"))
