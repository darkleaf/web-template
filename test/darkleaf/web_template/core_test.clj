(ns darkleaf.web-template.core-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]
   [darkleaf.web-template.protocols :as wtp]
   [darkleaf.web-template.writer :as w]))

(defmacro test-tmpl
  {:style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string (wt/compile (quote dsl#))
                                                       data#))
       ~@body)))




(t/deftest body-test
  (test-tmpl
    [div "a"]
    nil
    "<div>a</div>"

    [div 1]
    nil
    "<div>1</div>"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(t/deftest string-tag
  (test-tmpl
    ["ns:tag"]
    nil
    "<ns:tag></ns:tag>"

    ["x.y.z"]
    nil
    "<x.y.z></x.y.z>"

    ["$tag$"]
    nil
    "<$tag$></$tag$>"))

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
    "<div class=\"a b\"></div>"

    [div {(:attr) "foo"}]
    {:attr "class"}
    "<div class=\"foo\"></div>"

    [.a {class (:class)
         ...   (:attrs)}]
    {:class "b"
     :attrs {"class" "c"}}
    "<div class=\"a b c\"></div>"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(comment
 (defn- render [node data]
   (let [tmpl (wt/compile* node)]
     (wt/render-to-string tmpl data)))

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
     "<div>c b* a*</div>")))
