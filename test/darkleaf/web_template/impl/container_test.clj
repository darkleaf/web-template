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
    {'this nil}
    ""

    (this "present")
    {'this nil}
    ""

    (this "present" "blank")
    {'this nil}
    "blank"))

(t/deftest string-test
  (test-tmpl
    (this)
    {'this "a"}
    "a"

    (this (this))
    {'this "a"}
    "a"

    (this "present")
    {'this "a"}
    "present"

    (this "present")
    {'this ""}
    ""

    (this "present" "blank")
    {'this "a"}
    "present"

    (this "present" "blank")
    {'this ""}
    "blank"))

(t/deftest number-test
  (test-tmpl
    (this "present" "blank")
    {'this 1}
    "present"

    (this "present" "blank")
    {'this 0}
    "blank"

    (this "present" "blank")
    {'this 0.0}
    "blank"))

(t/deftest boolean-test
  (test-tmpl
    (this)
    {'this true}
    "true"

    (this)
    {'this false}
    "false"

    (this (this))
    {'this true}
    "true"

    (this "present")
    {'this true}
    "present"

    (this "present")
    {'this false}
    ""

    (this "present" "blank")
    {'this true}
    "present"

    (this "present" "blank")
    {'this false}
    "blank"))

(t/deftest vector-test
  (test-tmpl
    #_#_#_
    (this)
    {'this []}
    ""

    #_#_#_
    (this)
    {'this [true false]}
    "[true false]"

    (this [<> (this) " "])
    {'this ["a" "b"]}
    "a b "

    (this "present ")
    {'this [true false]}
    "present present "

    (this "present ")
    {'this []}
    ""

    (this "present " "blank")
    {'this [true false]}
    "present present "

    (this "present " "blank")
    {'this []}
    "blank"))

(t/deftest set-test
  (test-tmpl
    #_#_#_
    (this)
    {'this #{}}
    "#{}"

    #_#_#_
    (this)
    {'this #{true false}}
    "#{true false}"

    (this [<> (this) " "])
    {'this #{"a" "b"}}
    "a b "

    (this "present ")
    {'this #{true false}}
    "present present "

    (this "present ")
    {'this #{}}
    ""

    (this "present " "blank")
    {'this #{true false}}
    "present present "

    (this "present " "blank")
    {'this #{}}
    "blank"))

(t/deftest map-test
  (test-tmpl
    #_#_#_
    (this)
    {'this {}}
    "{}"

    #_#_#_
    (this)
    {'this {:a "value"}}
    "{:a &quot;value&quot;}"

    #_#_#_
    (this (this))
    {'this {:a :b}}
    "{:a :b}"

    (this (:a))
    {'this {:a "value"}}
    "value"

    (this (:a))
    {'this {}}
    ""

    (this "present")
    {'this {}}
    ""

    (this "present" "blank")
    {'this {}}
    "blank"

    (this "present" "blank")
    {'this {:a "value"}}
    "present"))

(t/deftest object-test
  (let [obj (reify Object
              (toString [_]
                "obj"))]
    (test-tmpl
      (this)
      {'this obj}
      "obj"

      (this (this))
      {'this obj}
      "obj"

      (this "present")
      {'this obj}
      "present"

      (this "present" "blank")
      {'this obj}
      "present")))
