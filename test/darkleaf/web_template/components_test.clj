(ns darkleaf.web-template.components-test
  (:require
   [darkleaf.web-template.core :as wt]
   [darkleaf.web-template.protocols :as wtp]
   [darkleaf.web-template.components :as c]
   [clojure.test :as t]))

(defn- render [node ctx data]
  (let [tmpl (wt/compile node)]
    (wt/render-to-string tmpl ctx data)))

(defmacro test-tmpl
  {:private true
   :style/indent :defn}
  [ctx & body]
  (when (seq body)
    `(t/are [node# data# html#] (= html# (render (quote node#) ~ctx data#))
       ~@body)))

(t/deftest format-test
  (test-tmpl {:format c/format}
    (:format {:fmt "%b"})
    false
    "false"

    (:format {:fmt "%b"})
    true
    "true"

    (:format {:fmt "%b" 1 (:a)})
    {:a false}
    "false"

    (:format {:fmt "%d %d" 1 (:a) 2 (:b)})
    {:a 10 :b 20}
    "10 20"

    (:format {:fmt "%d %d %d" 1 (:a) 2 (:b) 3 (:c)})
    {:a 10 :b 20 :c 30}
    "10 20 30"

    (:format {:fmt "%d %d %d %d" 1 (:a) 2 (:b) 3 (:c) 4 (:d)})
    {:a 10 :b 20 :c 30 :d 40}
    "10 20 30 40"


    (:format {:fmt "x: %d"}
             [div .])
    42
    "<div>x: 42</div>"))
