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

    [div [div]]
    nil
    "<div><div></div></div>"))

(t/deftest nil-test
  (test-tmpl
    .
    nil
    "nil"

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
    "[]\n"

    .
    [true false]
    "[true false]\n"

    .
    (list)
    "()\n"

    .
    (list true)
    "(true)\n"

    (. .)
    [true false]
    "true false "

    (. . "empty")
    [true false]
    "true false "

    (. . "empty")
    []
    "empty"))

(t/deftest map-test
  (test-tmpl
    .
    {}
    "{}\n"

    .
    {:a "value"}
    "{:a \"value\"}\n"

    (. (:a))
    {:a "value"}
    "value"

    (. (:a) "empty")
    {:a "value"}
    "value"

    (. (:a) "empty")
    {}
    "empty"))

(t/deftest branch-ctx-test
  (t/are [active? html] (= html (render '(:active? (:login) (:error-msg))
                                        {:active?   active?
                                         :login     "john"
                                         :error-msg "not active"}))
    true
    "john"

    false
    "not active"

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
    "<div>a</div> <div>b</div> "

    (:a (:b [div (:c) (:b*) (:a*)]))
    {:a  {:b  {:c "c"}
          :b* "b*"}
     :a* "a*"}
    "<div>cb*a*</div>"))


(t/deftest special-tags-test
  (test-tmpl
    [<>
     [div "a"]
     [div "b"]]
    nil
    "<div>a</div> <div>b</div> "

    (.) ;; -> .
    "a"
    "a"

    .
    "a"
    "a"))


'[div {class (:class)}]
'[div {(:attr) true}]
'[div (:attrs)]
'[div {data {foo "43"}}]


'[.foo#bar {data {(:segment) {foo  (:x)
                              (:y) "bar"
                              "xyz" "42"
                              "abc" ["a" "b" (:c)]}}}]
