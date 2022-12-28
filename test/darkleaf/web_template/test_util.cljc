(ns darkleaf.web-template.test-util
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt])
  #?(:cljs (:require-macros [darkleaf.web-template.test-util :refer [test-tmpl]])))

(defmacro test-tmpl
  {:style/indent :defn}
  [mode & body]
  (let [[mode body] (if (symbol? mode)
                      [mode body]
                      [wt/html5-mode (cons mode body)])]
    `(t/are [dsl# data# html#] (= html#
                                  (wt/render-to-string
                                   (merge data#
                                          {::wt/renderable (wt/compile (quote dsl#) ~mode)})))
       ~@body)))
