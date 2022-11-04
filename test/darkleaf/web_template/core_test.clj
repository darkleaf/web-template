(ns darkleaf.web-template.core-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]
   [darkleaf.web-template.protocols :as p]))

(defn- render [node data]
  (let [tmpl (wt/compile* node)]
    (wt/render-to-string tmpl data)))

(defmacro test-tmpl
  {:private true
   :style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string
                                   (wt/compile dsl#) data#))
       ~@body)))


;; todo: inverted block ctx
;; (:a . (:debug)))
;; :debug is a component. It can access '. with is a {:a :value}

(t/deftest static-test
  (test-tmpl
    "a"
    nil
    "a"

    [div]
    nil
    "<div></div>"

    [div "a"]
    nil
    "<div>a</div>"

    [div "a" "b"]
    nil
    "<div>a b</div>"

    [div [div]]
    nil
    "<div><div></div></div>"))

(t/deftest body-test
  (test-tmpl
   [div "a"]
   nil
   "<div>a</div>"

   [div 1]
   nil
   "<div>1</div>"))

(t/deftest nil-test
  (test-tmpl
    .
    nil
    ""

    (. "present")
    nil
    ""

    ^:blank (. "blank")
    nil
    "blank"))

(t/deftest string-test
  (test-tmpl
    .
    "a"
    "a"

    (. "present")
    "a"
    "present"

    (. "present")
    ""
    ""

    ^:blank (. "blank")
    ""
    "blank"

    ^:blank (. "blank")
    "a"
    ""))

(t/deftest boolean-test
  (test-tmpl
    .
    true
    "true"

    .
    false
    "false"

    (. "present")
    true
    "present"

    (. "present")
    false
    ""

    ^:blank (. "blank")
    false
    "blank"

    ^:blank (. "blank")
    true
    ""))

(t/deftest seq-test
  (test-tmpl
    .
    []
    "[]"

    .
    [true false]
    "[true false]"

    .
    (list)
    "()"

    .
    (list true)
    "(true)"

    (. "present")
    [true false]
    "present present "

    ^:blank (. "blank")
    [true false]
    ""

    ^:blank (. "blank")
    []
    "blank"

    ^:blank (. "blank")
    [true false]
    ""))

(t/deftest map-test
  (test-tmpl
    .
    {}
    "{}"

    .
    {:a "value"}
    "{:a \"value\"}"

    (. (:a))
    {:a "value"}
    "value"

    (. (:a))
    {:a "value"}
    "value"

    (. (:a))
    {}
    ""

    ^:blank (. "blank")
    {}
    "blank"

    ^:blank (. "blank")
    {:a "value"}
    ""))

(t/deftest object-test
  (let [obj (reify Object
              (toString [_]
                "obj"))]
    (test-tmpl
      .
      obj
      "obj"

      (. "present")
      obj
      "present"

      ^:blank (. "blank")
      obj
      "")))

(t/deftest default-inverted-block-test
  (t/are [data] (= "" (render '(. "block")
                              data))
    nil
    false
    ""
    []
    '()
    {}))

(def tmpl
  (wt/compile
   ^:blank (. "blank")))

(let [x (quote ^:x (1 2 3))]
  (meta x))

(t/deftest branch-ctx-test
  (t/are [active? html]
      (= html
         #_(let [;#_#_
                 tmpl
                 #_#_
                 tmpl tmpl
                 data  false
                 #_ {:active?   active?
                     :login     "john"
                     :error-msg "not active"}])
         42
         (wt/render-to-string
          (wt/compile
           ^:blank (. "blank"))
          false))
    true
    "john"

    false
    "not active"))

    ;; ""
    ;; "not active"

    ;; []
    ;; "not active"

    ;; {}
    ;; "not active"))

(t/deftest case-0-test
  (test-tmpl
    [div (:name)]
    {:name "a"}
    "<div>a</div>"

    (:users
     [div (:login)])
    {:users [{:login "a"}
             {:login "b"}]}
    "<div>a</div> <div>b</div>"

    (:a (:b [div (:c) (:b*) (:a*)]))
    {:a  {:b  {:c "c"}
          :b* "b*"}
     :a* "a*"}
    "<div>c b* a*</div>"))


(t/deftest special-tags-test
  (test-tmpl
    [<>
     [div "a"]
     [div "b"]]
    nil
    "<div>a</div> <div>b</div>"

    (.) ;; -> .
    "a"
    "a"

    .
    "a"
    "a"))

(t/deftest literal-attributes
  (test-tmpl
    [.a]
    nil
    "<div class=\"a\"></div>"

    [.a.b]
    nil
    "<div class=\"a b\"></div>"

    [:#a]
    nil
    "<div id=\"a\"></div>"

    [.a#b]
    nil
    "<div id=\"b\" class=\"a\"></div>"))

(t/deftest static-attrs
  (test-tmpl
    [div {class "a"}]
    nil
    "<div class=\"a\"></div>"

    [div {class a}]
    nil
    "<div class=\"a\"></div>"

    [div {:class :a}]
    nil
    "<div class=\"a\"></div>"

    [div {class [a b]}]
    nil
    "<div class=\"a b\"></div>"

    [div {class #{a b}}]
    nil
    "<div class=\"a b\"></div>"

    #_#_#_
    nil true false

    [div {class foo/a}]
    nil
    "<div class=\"a\"></div>"

    [div {class :foo/a}]
    nil
    "<div class=\"a\"></div>"))

(t/deftest dynamic-attrs
  (test-tmpl
    [.a {class (:class)}]
    {:class "b"}
    "<div class=\"b\"></div>"

    [div {(:attr) "foo"}]
    {:attr "class"}
    "<div class=\"foo\"></div>"

    [.a {class (:class)
         ...   (:attrs)}]
    {:class "b"
     :attrs {"class" "c"}}
    "<div class=\"c\"></div>"))

(t/deftest template-as-component-1
  (let [layout (wt/compile
                [layout
                 (:body {:param "xyz"})])
        page   (wt/compile
                [page (:param)])]
    (t/is (= "<layout><page>xyz</page></layout>"
             (wt/render-to-string layout {:body page})))))

(t/deftest template-as-component-3
  (let [layout (wt/compile
                [layout
                 (:body {:param "param"}
                        "block"
                        "inverted block")])
        page   (wt/compile
                [page
                 (:param)
                 (:block)
                 (:inverted-block)])]
    (t/is (= "<layout><page>param block inverted block</page></layout>"
             (wt/render-to-string layout {:body page})))))

(t/deftest compile-component-attrs
  (let [layout (wt/compile
                [layout {class (:class)}
                 [body (:body)]
                 [sidebar (:sidebar)]])
        tmpl   (wt/compile
                (:layout {:body    ^:tmpl [div "body"]
                          :sidebar ^:tmpl [div "sidebar"]
                          :class   [a b]}))]
    (t/is (= "<layout class=\"a b\"><body><div>body</div></body> <sidebar><div>sidebar</div></sidebar></layout>"
             (wt/render-to-string tmpl {:layout layout})))))
