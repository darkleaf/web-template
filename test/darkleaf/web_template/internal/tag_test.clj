;; *********************************************************************
;; * Copyright (c) 2022 Mikhail Kuzmin
;; *
;; * This program and the accompanying materials are made
;; * available under the terms of the Eclipse Public License 2.0
;; * which is available at https://www.eclipse.org/legal/epl-2.0/
;; *
;; * SPDX-License-Identifier: EPL-2.0
;; **********************************************************************/

(ns darkleaf.web-template.internal.tag-test
  (:require
   [darkleaf.web-template.internal.tag :refer [parse-tag]]
   [clojure.test :as t]))

(t/deftest parse-tag-test
  (t/are [literal tag attrs] (t/is (= [tag attrs] (parse-tag (quote literal))))
    "foo"
    "foo" nil

    "foo:bar"
    "foo:bar" nil

    "foo#bar"
    "foo#bar" nil

    div
    "div" nil

    span
    "span" nil

    div.a
    "div" {"class" "a"}

    div#a
    "div" {"id" "a"}

    .a
    "div" {"class" "a"}

    .a#b
    "div" {"class" "a", "id" "b"}

    .a#b.c
    "div" {"class" "a c", "id" "b"}

    :#a
    "div" {"id" "a"}

    x/tag
    "x:tag" nil

    x/tag.a
    "x:tag" {"x:class" "a"}))
