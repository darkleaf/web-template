(ns darkleaf.web-template.core-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]
   [darkleaf.web-template.protocols :as wtp]
   [darkleaf.web-template.internal.writer :as w]))

(defmacro test-tmpl
  {:private      true
   :style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string (wt/compile dsl#)
                                                       data#))
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
    "<div><div></div></div>"

    [:div]
    nil
    "<div></div>"

    ["div"]
    nil
    "<div></div>"))


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
    "<div class=\"b\"></div>"

    [div {(:attr) "foo"}]
    {:attr "class"}
    "<div class=\"foo\"></div>"

    [.a {class (:class)
         ...   (:attrs)}]
    {:class "b"
     :attrs {"class" "c"}}
    "<div class=\"c\"></div>"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(t/deftest <>-test
  (test-tmpl
    [<>
     [div "a"]
     [div "b"]]
    nil
    "<div>a</div> <div>b</div>"))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(t/deftest nil-test
  (test-tmpl
    (this)
    nil
    ""

    (this "present")
    nil
    ""

    (this "present" "blank")
    nil
    "blank"))

(t/deftest string-test
  (test-tmpl
    (this)
    "a"
    "a"

    (this (this))
    "a"
    "a"

    (this "present")
    "a"
    "present"

    (this "present")
    ""
    ""

    (this "present" "blank")
    "a"
    "present"

    (this "present" "blank")
    ""
    "blank"))

(t/deftest boolean-test
  (test-tmpl
    (this)
    true
    "true"

    (this)
    false
    "false"

    (this (this))
    true
    "true"

    (this "present")
    true
    "present"

    (this "present")
    false
    ""

    (this "present" "blank")
    true
    "present"

    (this "present" "blank")
    false
    "blank"))


(t/deftest seq-test
  (test-tmpl
    ;; todo? [1 2 3] -> "1, 2, 3"
    (this)
    []
    "[]"

    (this)
    [true false]
    "[true false]"

    (this)
    (list)
    "()"

    (this)
    (list true)
    "(true)"

    (this (this))
    ["a" "b"]
    "a b "

    (this "present")
    [true false]
    "present present "

    (this "present")
    []
    ""

    (this "present" "blank")
    [true false]
    "present present "

    (this "present" "blank")
    []
    "blank"))

(t/deftest map-test
  (test-tmpl
    (this)
    {}
    "{}"

    (this)
    {:a "value"}
    "{:a \"value\"}"

    (this (this))
    {:a :b}
    "{:a :b}"

    (this (:a))
    {:a "value"}
    "value"

    (this (:a))
    {:a "value"}
    "value"

    (this (:a))
    {}
    ""

    (this "present")
    {}
    ""

    (this "present" "blank")
    {}
    "blank"

    (this "present" "blank")
    {:a "value"}
    "present"))

(t/deftest object-test
  (let [obj (reify Object
              (toString [_]
                "obj"))]
    (test-tmpl
      (this)
      obj
      "obj"

      (this (this))
      obj
      "obj"

      (this "present")
      obj
      "present"

      (this "present" "blank")
      obj
      "present")))

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
    (t/is (= "<layout><body>my body</body> <section>my section</section></layout>"
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

(def compiled-part (wt/compile [inner "partial"]))

(t/deftest var-as-partial-test
  (let [tmpl (wt/compile [outer #'compiled-part])]
    (t/is (= "<outer><inner>partial</inner></outer>"
             (wt/render-to-string tmpl nil)))
    (with-redefs [compiled-part (wt/compile [inner "redefined"])]
      (t/is (= "<outer><inner>redefined</inner></outer>"
             (wt/render-to-string tmpl nil))))))

(t/deftest fn-as-renderable-test
  (let [admin   (wt/compile "admin")
        user    (wt/compile "user")
        default (wt/compile "undefined user type")
        f       (fn [ctx]
                  (case (-> ctx :user :type)
                    :admin admin
                    :user  user
                    default))
        tmpl    (wt/compile
                 (:user ~f))
        data    {:user {:type :admin}}]
    (t/is (= "admin" (wt/render-to-string tmpl data)))))

(t/deftest fn-as-attr-value-test
  (let [classes (fn [ctx]
                  (case (-> ctx :user :type)
                    :admin [:base :admin]
                    :user  [:base :user]))
        tmpl    (wt/compile
                 (:user
                  [div {class ~classes}
                   (:name)]))
        data    {:user {:type :admin
                        :name "John"}}]
    (t/is (= "<div class=\"base admin\">John</div>"
             (wt/render-to-string tmpl data)))))

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
