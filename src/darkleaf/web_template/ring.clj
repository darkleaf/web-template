(ns darkleaf.web-template.ring
  (:require
   [darkleaf.web-template.protocols :as wtp]
   [ring.core.protocols :as rp])
  (:import
   (java.io OutputStream OutputStreamWriter)))

(set! *warn-on-reflection* true)

(defn body [template data]
  (reify rp/StreamableResponseBody
    (write-body-to-stream [_ _ os]
      (with-open [os  ^OutputStream os
                  osw (OutputStreamWriter. os)]
        (wtp/render template osw data)))))

;; todo? BufferedWriter(new OutputStreamWriter(out))
