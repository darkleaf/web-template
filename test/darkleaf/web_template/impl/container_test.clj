(ns darkleaf.web-template.impl.container-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]))

(defmacro test-tmpl
  {:style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string (wt/compile (quote dsl#))
                                                       data#))
       ~@body)))

;; todo: (this) это не про container

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

(t/deftest number-test
  (test-tmpl
    (this "present" "blank")
    1
    "present"

    (this "present" "blank")
    0
    "blank"

    (this "present" "blank")
    0.0
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

(t/deftest vector-test
  (test-tmpl
    #_#_#_
    (this)
    []
    ""

    #_#_#_
    (this)
    [true false]
    "[true false]"

    (this [<> (this) " "])
    ["a" "b"]
    "a b "

    (this "present ")
    [true false]
    "present present "

    (this "present ")
    []
    ""

    (this "present " "blank")
    [true false]
    "present present "

    (this "present " "blank")
    []
    "blank"))

(t/deftest set-test
  (test-tmpl
    #_#_#_
    (this)
    #{}
    "#{}"

    #_#_#_
    (this)
    #{true false}
    "#{true false}"

    (this [<> (this) " "])
    #{"a" "b"}
    "a b "

    (this "present ")
    #{true false}
    "present present "

    (this "present ")
    #{}
    ""

    (this "present " "blank")
    #{true false}
    "present present "

    (this "present " "blank")
    #{}
    "blank"))

(t/deftest map-test
  (test-tmpl
    #_#_#_
    (this)
    {}
    "{}"

    #_#_#_
    (this)
    {:a "value"}
    "{:a &quot;value&quot;}"

    #_#_#_
    (this (this))
    {:a :b}
    "{:a :b}"

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
