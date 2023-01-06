;; # Web template
;; Logic-less templates for Clojure.

;; ## 🚦 Status
;; Beta.
;; Feature researh has been completed.
;; I'm going to use it in production in the fall of 2023.

;; ## 🔌 Installation
;; ```clojure
;; ;; deps.edn
;; {:deps {org.clojars.darkleaf/web-templates
;;         {:git/url "https://github.com/darkleaf/web-template.git"}]))
;;          :sha     "4f65c6537868f103078a1b38a249732f742d1b0f"}}}
;; ```

;; ## 🚀 Usage
;;
;; As a first approximation, WebTemplate uses Hiccup syntax.
;; You can use symbols, keywords, and string to define tags.
;; WebTemplate provides a CSS-like shortcut for denoting id and class attributes.
;;
;; WebTemplate is logic-less template engine like [Mustache](https://mustache.github.io/mustache.5.html).
;; It uses
;; + `(:key)` like `{{ key }}`
;; + `(:key 👍)` like `{{key}}👍{{/key}}`.
;; + `(:key 👍 👎)` like `{{key}}👍{{/key}}{{^key}}👎{{/key}}`.

(ns notebooks.readme
  {:nextjournal.clerk/toc true}
  (:require
   [darkleaf.web-template.core :as wt]
   [nextjournal.clerk :as clerk]
   [clojure.edn :as edn]))

;; ### 🗿 Static template

;; Let's define first template.

(def hello-world
  (wt/compile
   '[.text-red-500 "Hello world!"]))

;; To render the template use `wt/render-to-string`:

(wt/render-to-string hello-world)

;; ### 🙋‍♀️ Value substitution

(def user-greeting
  (wt/compile
   '[div "Hello " (:name)]))

(-> {::wt/renderable user-greeting
     :name           "Jane"}
    wt/render-to-string clerk/html)

;; ### 🦺 Safe by default

(-> {::wt/renderable user-greeting
     :name           "<strong>Jane</strong>"}
    wt/render-to-string clerk/html)

;; ### 🤷‍♂️ Value substitutin with default
(def user-greeting-2
  (wt/compile
   '[div "Hello " (:name (this) "🤷‍♂️")]))

(-> {::wt/renderable user-greeting-2
     :name           "Jane"}
    wt/render-to-string clerk/html)

(-> {::wt/renderable user-greeting-2
     :name           ""}
    wt/render-to-string clerk/html)

;; ### 📦 Containers

(def containers
  (wt/compile
   '[ul
     (:items
      [li (:title) ": " (:value (this) "🤷‍♀️")])]))

(-> {::wt/renderable containers
     :items          [{:title "nil" :value nil}
                      {:title "not zero" :value 1}
                      {:title "zero" :value 0}
                      {:title "present string" :value "123"}
                      {:title "blank string" :value ""}
                      {:title "empty coll" :value []}
                      {:title "coll" :value (interpose ", " [1 2 3])}
                      {:title "template" :value (wt/compile '[strong "template"])}]}
    wt/render-to-string clerk/html)

;; ### Presenter

{::clerk/visibility {:result :hide}}

(def table-row
  (wt/compile
   '[tr
     [td (:id)]
     [td (:name)]]))

(defn table-row-presenter [{:keys [id name]}]
  {::wt/renderable table-row
   :id             id
   :name           name})

(def table
  (wt/compile
   '[table
     [thead
      [tr
       [th "id"]
       [th "name"]]]
     [tbody
      (:rows)]]))

(defn table-presenter [users]
  {::wt/renderable table
   :rows           (map table-row-presenter users)})

(def users
  [{:id 1 :name "Jonh"}
   {:id 2 :name "Jane"}])

{::clerk/visibility {:result :show}}
(-> (table-presenter users)
    wt/render-to-string clerk/html)

;; ### Attributes

(-> '[div.a {class b}]
    wt/compile wt/render-to-string)

(-> '[div.a {class [b c]}]
    wt/compile wt/render-to-string)

(-> '[div.a {class {b true c false}}]
    wt/compile wt/render-to-string)

(-> {::wt/renderable (wt/compile
                      '[div {class (:class)} 42])
     :class "a"}
    wt/render-to-string)

(-> {::wt/renderable (wt/compile
                      '[div {class (:class)} 42])
     :class {:a true :b false}}
    wt/render-to-string)

(-> {::wt/renderable (wt/compile
                      '[div {... (:attrs)} 42])
     :attrs {:class {:a true :b false}}}
    wt/render-to-string)

;; ### `<>` tag

(-> '[<>
      "<!-- comment -->"
      [.foo 🧜‍♂️]
      [.bar 🧜‍♀️]]
    wt/compile wt/render-to-string)

;; ### Namespaces

(-> '[html/div.foo "🥷"]
    wt/compile wt/render-to-string)

(-> '["html:div" "🥷"]
    wt/compile wt/render-to-string)

;; ### HTML5 vs XML

(-> '[img {src "https://placekitten.com/200/200"}]
    (wt/compile) wt/render-to-string)

(-> '[img {src "https://placekitten.com/200/200"}]
    (wt/compile wt/xml-mode) wt/render-to-string)

(-> '[input {required true}]
    wt/compile wt/render-to-string)

(-> '[input {required true}]
    (wt/compile wt/xml-mode) wt/render-to-string)

;; ### 🦸‍♂️ EDN for template and data

(let [tmpl "[div \"Hello \" (:name)]"
      data "{:name \"John\"}"
      tmpl (-> tmpl edn/read-string wt/compile)
      data (-> data edn/read-string)]
  (-> data
      (assoc ::wt/renderable tmpl)
      (wt/render-to-string)))

;; ## API

;;; ### `wt/compile`
(-> #'wt/compile meta :arglists)
(-> #'wt/compile meta :doc #_clerk/md)

;;; todo: ring body
