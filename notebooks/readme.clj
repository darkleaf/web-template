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
   [nextjournal.clerk :as clerk]))

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
    (wt/render-to-string)
    (clerk/html))
