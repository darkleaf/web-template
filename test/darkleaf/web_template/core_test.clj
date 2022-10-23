(ns darkleaf.web-template.core-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(defn- render [node data]
  (let [tmpl (wt/compile node)]
    (wt/render-to-string tmpl data)))

(defmacro test-tmpl
  {:private true
   :style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [node# data# html#] (= html# (render (quote node#) data#))
       ~@body)))

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

(t/deftest nil-test
  (test-tmpl
    .
    nil
    ""

    (. .)
    nil
    ""

    (. "a" "b")
    nil
    "b"))

(t/deftest string-test
  (test-tmpl
    .
    "a"
    "a"

    (. .)
    "a"
    "a"

    (. . "b")
    "a"
    "a"

    (. . "b")
    ""
    "b"

    #_(. {:escape false} . "not-found")))

(t/deftest boolean-test
  (test-tmpl
    .
    true
    "true"

    .
    false
    "false"

    (. .)
    true
    "true"

    (. "a" "b")
    true
    "a"

    (. "a" "b")
    false
    "b"))

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

    (. .)
    [true false]
    "true false"

    (. . "empty")
    [true false]
    "true false"

    (. . "empty")
    []
    "empty"

    (. {:separator ", "} .)
    ["a" "b"]
    "a, b"))

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

    (. (:a) "empty")
    {:a "value"}
    "value"

    (. (:a) "empty")
    {}
    "empty"))

(t/deftest object-test
  (let [obj (reify Object
              (toString [_]
                "obj"))]
    (test-tmpl
      .
      obj
      "obj"

      (. .)
      obj
      "obj"

      (. . "empty")
      obj
      "obj")))

(t/deftest default-inverted-block-test
  (t/are [data] (= "" (render '(. "block")
                              data))
    nil
    false
    ""
    []
    '()
    {}))

#_(t/deftest format-test
    (test-tmpl
      (. {:format "%b"})
      nil
      "false"))

(t/deftest branch-ctx-test
  (t/are [active? html] (= html (render '(:active? (:login) (:error-msg))
                                        {:active?   active?
                                         :login     "john"
                                         :error-msg "not active"}))
    true
    "john"

    false
    "not active"

    ""
    "not active"

    []
    "not active"

    {}
    "not active"))

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

(t/deftest template-as-component-1
  (let [layout (wt/compile
                '[layout
                  (:body {:param "xyz"})])
        page   (wt/compile
                '[page (:param)])]
    (t/is (= "<layout><page>xyz</page></layout>"
             (wt/render-to-string layout {:body page})))))

(t/deftest template-as-component-3
  (let [layout (wt/compile
                '[layout
                  (:body {:param "param"}
                         "block"
                         "inverted block")])
        page   (wt/compile
                '[page
                  (:param)
                  (:block)
                  (:inverted-block)])]
    (t/is (= "<layout><page>param block inverted block</page></layout>"
             (wt/render-to-string layout {:body page})))))

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

#_(t/deftest merge-attrs
    (test-tmpl
      [.a {class "b"}]
      nil
      "<div class=\"b\"></div>"


      ;; [] будут заняты под шаблон
      ;; а атрибутам вроде как не важен порядок
      #_#_#_
      [div {class #{a b}}]
      nil
      "<div class=\"a\"></div>"

      [.a {class "b"}]
      nil
      "<div class=\"a b\"></div>"))

(t/deftest dynamic-attrs
  (test-tmpl
    [.a {class (:class)}]
    {:class "b"}
    "<div class=\"b\"></div>"

    ;; слишком сложно
    #_[div {(:attr) "foo"}]

    [.a {class (:class)
         ...   :attrs}]
    {:class "b"
     :attrs {"class" "c"}}
    "<div class=\"c\"></div>"))

#_"
или если в контексте лежит
  * строка, symbol, keyword, bool, num - то это точно замена
  * коллекция - это это добавление
  * nil - не трогаем ?? или удаляем ??
  * false - удаляем ??
"

(comment
  [div {data-controller users/list-item}]
  users--list-item)

;; вообще это больно "умно" будет, наверное не нужно так
;; <div data-controller="gallery"
;;                       ^^^^^^^
;;      data-action="resize@window->gallery#layout">
;;                                  ^^^^^^^ resize->user--list-item
;; </div>


(comment
  [div {... ...}
   ...]

  (comp
   {... ...}
   ...)

  '(stimulus/controller
    {name  user/search
     class #{foo bar buz}}
    :aaa (stimulus/target {tag li} xyz)
    :bb (...))

  '(^{:style/indent :defn} stimulus/controller {name  user/search
                                                class #{foo bar buz}}
    (stimulus/target {tag li} xyz))

  ;; даже не смотря на ', если подключить wt, то cider считает :style/indent
  '(wt/xyz {}
     xxx)

  '(defn xyz []
     123))

(comment
  '(:layout {:sidebar [div ...
                       [div ...]]
             :body [div ...
                    [div ...]]})

  '[div {:class [foo bar]}]

  '[div {:class #{foo bar}}]
  '[.foo.bar])

(comment
  [div {class aa, id bb, ... attrs}
   "a"
   "b"]

  [div class aa, id bb
   & [<>
      "block"
      "inverted block"]
   attrs]

  (:users
   {:separator ", ", ... attrs}
   [.title (:title)]
   "no users")

  (:users
   :separator ", "
   & [.title (:title)]
   | "no users"
   attrs))
