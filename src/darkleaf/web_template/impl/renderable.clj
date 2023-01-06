;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.impl.renderable
  (:require
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]
   [darkleaf.web-template.core :as-alias wt]))

(defn render-seqable [this w ctx]
  (doseq [item this]
    (p/render item w ctx)))

(defn render-default [this w ctx]
  (w/append w (str this)))

(defn render-map [this w ctx]
  (if-some [template (::wt/renderable this)]
    (p/render template
              w
              (merge ctx
                     this
                     {'this (dissoc this ::wt/renderable)}))
    (render-default this w ctx)))

(extend-protocol p/Renderable
  nil
  (render [_ _ _])

  #_#_
  clojure.lang.Var
  (render [this w ctx]
    (p/render @this w ctx))

  Object
  (render [this w ctx]
    (cond
      (map? this)     (render-map this w ctx)
      (seqable? this) (render-seqable this w ctx)
      :default        (render-default this w ctx))))
