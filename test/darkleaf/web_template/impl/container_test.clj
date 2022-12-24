(ns darkleaf.web-template.impl.container-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]))

(defmacro test-tmpl
  {:style/indent :defn}
  [& body]
  (when (seq body)
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string
                                   (merge data#
                                          {::wt/renderable (wt/compile (quote dsl#))})))
       ~@body)))

;; todo: (this) это не про container

(t/deftest nil-test
  (test-tmpl
    (:value)
    {:value nil}
    ""

    (:value "present")
    {:value nil}
    ""

    (:value "present" "blank")
    {:value nil}
    "blank"))

(t/deftest string-test
  (test-tmpl
    (:value)
    {:value "a"}
    "a"

    (:value (:value))
    {:value "a"}
    "a"

    (:value "present")
    {:value "a"}
    "present"

    (:value "present")
    {:value ""}
    ""

    (:value "present" "blank")
    {:value "a"}
    "present"

    (:value "present" "blank")
    {:value ""}
    "blank"))

(t/deftest number-test
  (test-tmpl
    (:value "present" "blank")
    {:value 1}
    "present"

    (:value "present" "blank")
    {:value 0}
    "blank"

    (:value "present" "blank")
    {:value 0.0}
    "blank"))

(t/deftest boolean-test
  (test-tmpl
    (:value)
    {:value true}
    "true"

    (:value)
    {:value false}
    "false"

    (:value (:value))
    {:value true}
    "true"

    (:value "present")
    {:value true}
    "present"

    (:value "present")
    {:value false}
    ""

    (:value "present" "blank")
    {:value true}
    "present"

    (:value "present" "blank")
    {:value false}
    "blank"))

(t/deftest vector-test
  (test-tmpl
    #_#_#_
    (:value)
    {:value []}
    ""

    #_#_#_
    (:value)
    {:value [true false]}
    "[true false]"

    (:value [<> (this) " "])
    {:value ["a" "b"]}
    "a b "

    (:value "present ")
    {:value [true false]}
    "present present "

    (:value "present ")
    {:value []}
    ""

    (:value "present " "blank")
    {:value [true false]}
    "present present "

    (:value "present " "blank")
    {:value []}
    "blank"))

(t/deftest set-test
  (test-tmpl
    #_#_#_
    (:value)
    {:value #{}}
    "#{}"

    #_#_#_
    (:value)
    {:value #{true false}}
    "#{true false}"

    (:value [<> (this) " "])
    {:value #{"a" "b"}}
    "a b "

    (:value "present ")
    {:value #{true false}}
    "present present "

    (:value "present ")
    {:value #{}}
    ""

    (:value "present " "blank")
    {:value #{true false}}
    "present present "

    (:value "present " "blank")
    {:value #{}}
    "blank"))

(t/deftest map-test
  (test-tmpl
    #_#_#_
    (:value)
    {:value {}}
    "{}"

    #_#_#_
    (:value)
    {:value {:a "value"}}
    "{:a &quot;value&quot;}"

    #_#_#_
    (:value (this))
    {:value {:a :b}}
    "{:a :b}"

    (:value (:a))
    {:value {:a "value"}}
    "value"

    (:value (:a))
    {:value {}}
    ""

    (:value "present")
    {:value {}}
    ""

    (:value "present" "blank")
    {:value {}}
    "blank"

    (:value "present" "blank")
    {:value {:a "value"}}
    "present"))

(t/deftest object-test
  (let [obj (reify Object
              (toString [_]
                "obj"))]
    (test-tmpl
      (:value)
      {:value obj}
      "obj"

      (:value (:value))
      {:value obj}
      "obj"

      (:value "present")
      {:value obj}
      "present"

      (:value "present" "blank")
      {:value obj}
      "present")))
