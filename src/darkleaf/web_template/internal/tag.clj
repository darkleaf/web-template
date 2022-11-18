(ns darkleaf.web-template.internal.tag
  (:require
   [clojure.string :as str]))

;; based on https://github.com/r0man/sablono/blob/master/src/sablono/normalize.cljc

(defn- strip-css
  "Strip the # and . characters from the beginning of `s`."
  [s]
  (str/replace s #"^[.#]" ""))

(defn- add-ns [ns s]
  (if ns
    (str ns ":" s)
    s))

(defn tag-namespace [tag]
  "Normalize `tag` into keyword or symbol."
  (cond
    (ident? tag)  tag
    (string? tag) (keyword tag)
    :else nil))

(defn parse-tag [s]
  (let [add-ns  (partial add-ns (namespace s))
        matches (re-seq #"[#.]?[^#.]+" (name s))
        [tag-name names]
        (cond (empty? matches)
              (throw (ex-info (str "Can't match CSS tag: " s) {:tag s}))

              (#{\# \.} (ffirst matches)) ;; shorthand for div
              ["div" matches]

              :default
              [(first matches) (rest matches)])
        tag-name         (add-ns tag-name)
        {id \# class \.} (group-by first names)
        id               (some->> id first strip-css)
        class            (some->> class (map strip-css) seq (str/join " "))
        attrs            (cond-> nil
                           id    (assoc (add-ns "id") id)
                           class (assoc (add-ns "class") class))]
    [tag-name attrs]))
