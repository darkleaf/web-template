(ns darkleaf.web-template.ring-test
  (:require
   [darkleaf.web-template.ring :as sut]
   [clojure.test :as t]
   [ring.core.protocols :as rp]
   [darkleaf.web-template.core :as wt])
  (:import
   (java.io ByteArrayOutputStream)))

(t/deftest ok
  (let [baos (ByteArrayOutputStream.)
        tmpl (wt/compile '(:value))
        body (sut/body {:value          "ok"
                        ::wt/renderable tmpl})]
    (rp/write-body-to-stream body nil baos)
    (t/is (= "ok" (str baos)))))
