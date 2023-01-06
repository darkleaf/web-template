;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.impl.container
  (:require
   [clojure.string :as str]
   [darkleaf.web-template.protocols :as p]
   [darkleaf.web-template.writer :as w]))

(defn container->renderable-map [this block inverted-block]
  (if (seq this)
    (reify p/Renderable
      (render [_ w ctx]
        (p/render block w (merge ctx this {'this this}))))
    inverted-block))

(defn container->renderable-seqable [this block inverted-block]
  (if (seq this)
    (reify p/Renderable
      (render [_ w ctx]
        (doseq [item this]
          (p/render block w (merge ctx
                                   (when (map? item) item)
                                   {'this item})))))
    inverted-block))

(defn container->renderable-default [this block inverted-block]
  (reify p/Renderable
    (render [_ w ctx]
      (p/render block w (assoc ctx 'this this)))))

(extend-protocol p/Container
  nil
  (container->renderable [this block inverted-block]
    inverted-block)

  Object
  (container->renderable [this b ib]
    (cond
      (map? this)     (container->renderable-map     this b ib)
      (seqable? this) (container->renderable-seqable this b ib)
      :default        (container->renderable-default this b ib)))

  String
  (container->renderable [this block inverted-block]
    (if-not (str/blank? this)
      (reify p/Renderable
        (render [_ w ctx]
          (p/render block w (assoc ctx 'this this))))
      inverted-block))

  Number
  (container->renderable [this block inverted-block]
    (if-not (zero? this)
      (reify p/Renderable
        (render [_ w ctx]
          (p/render block w (assoc ctx 'this this))))
      inverted-block))

  Boolean
  (container->renderable [this block inverted-block]
    (if this
      (reify p/Renderable
        (render [_ w ctx]
          (p/render block w (assoc ctx 'this this))))
      inverted-block)))
