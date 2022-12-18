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
                                  (wt/render-to-string (wt/compile dsl#)
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

(t/deftest partial-test
  (let [layout  (fn [body section]
                  (wt/compile
                   [layout
                    [body ~body]
                    [section ~section]]))
        body    (wt/compile
                 "my body")
        section (wt/compile
                 "my section")
        tmpl    (layout body section)]
    (t/is (= "<layout><body>my body</body><section>my section</section></layout>"
             (wt/render-to-string tmpl nil)))))

(defmacro with-us-locale [& body]
  `(let [previous# (java.util.Locale/getDefault)]
     (try
       (java.util.Locale/setDefault java.util.Locale/US)
       ~@body
       (finally
         (java.util.Locale/setDefault previous#)))))

(t/deftest helper-test
  (let [format (fn [fmt]
                 (reify wtp/Renderable
                   (render [_ w {this 'this}]
                     (w/append w (format fmt this)))))
        tmpl   (wt/compile
                [div (:price ~(format "%.2f"))])
        data   {:price 0.12345}]
    (with-us-locale
      (t/is (= "<div>0.12</div>"
               (wt/render-to-string tmpl data))))))


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
