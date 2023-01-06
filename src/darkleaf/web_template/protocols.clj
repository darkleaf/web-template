;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.protocols)

(defprotocol Renderable
  (render [this writer ctx]))

(defprotocol Element
  (element->renderable [this mode]))

(defprotocol Container
  (container->renderable [this block inverted-block]))

(defprotocol AttributeValue
  (attribute-value [this]))
