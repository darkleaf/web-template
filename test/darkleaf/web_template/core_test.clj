(ns darkleaf.web-template.core-test
  (:require
   [darkleaf.web-template.core :as wt]
   [clojure.test :as t]))

(t/deftest case-0-test
  (t/are [html node data] (= html (let [tmpl (wt/compile node)]
                                    (wt/render-to-string tmpl data)))
    "a"
    "a"
    nil

    "<div></div>"
    '[div]
    nil

    "<div>a</div>"
    '[div "a"]
    nil

    "<div><div></div></div>"
    '[div [div]]
    nil

    "<div>a</div>"
    '[div (:name)]
    {:name "a"}

    "<div>a</div><div>b</div>"
    '(:users
      [div (:login)])
    {:users [{:login "a"}
             {:login "b"}]}

    "a"
    '(:active?
      (:login))
    {:active? true
     :login   "a"}

    "not active"
    '(:active?
      (:login)
      "not active")
    {:active? false}

    "not active"
    '(:active?
      (:login)
      "not active")
    {:active? nil}

    "a"
    '(:user
      (:login))
    {:user {:login "a"}}

    "not found"
    '(:user
      (:login)
      "not found")
    {:user {}}

    "ab"
    '(. (.))
    ["a" "b"]

    "empty"
    '(. (.) "empty")
    []

    "<div>cb*a*</div>"
    '(:a (:b [div (:c) (:b*) (:a*)]))
    {:a  {:b  {:c "c"}
          :b* "b*"}
     :a* "a*"}

    "<div>a</div><div>b</div><div>c</div>"
    '[<>
      [div "a"]
      [div "b"]
      [div "c"]]
    nil))


'[div {class (:class)}]
'[div {(:attr) true}]
'[div (:attrs)]
'[div {data {foo "43"}}]


'[.foo#bar {data {(:segment) {foo  (:x)
                              (:y) "bar"
                              "xyz" "42"
                              "abc" ["a" "b" (:c)]}}}]
