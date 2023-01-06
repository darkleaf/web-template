;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.ring
  (:require
   [darkleaf.web-template.protocols :as wtp]
   [ring.core.protocols :as rp])
  (:import
   (java.io OutputStream OutputStreamWriter)))

(set! *warn-on-reflection* true)

(defn body [renderable]
  (reify rp/StreamableResponseBody
    (write-body-to-stream [_ _ os]
      (with-open [os  ^OutputStream os
                  osw (OutputStreamWriter. os)]
        (wtp/render renderable osw nil)))))

;; todo? BufferedWriter(new OutputStreamWriter(out))
