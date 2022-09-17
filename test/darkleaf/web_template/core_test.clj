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


    "<div><div>a</div><div>b</div></div>"
    '[div (:users
           [div (:login)])]
    {:users [{:login "a"}
             {:login "b"}]}))



'[div {class (:class)}]
'[div {(:attr) true}]
'[div (:attrs)]
'[div (:names (.) "no names")]