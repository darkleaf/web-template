(ns darkleaf.web-template.helpers-test
  (:require
   [clojure.test :as t]
   [darkleaf.web-template.core :as wt]
   [darkleaf.web-template.protocols :as wtp]))

;; я не до конца понимаю как это работает %)
#_(defmacro defhelper [name args body]
   (let [f (gensym "f_")]
     `(let [~f (fn ~args ~body)]
        (defmacro ~name ~args
          `(~~f ~~@(for [arg args]
                    (if (-> arg meta :compile)
                      ``(wt/compile ~~arg)
                      ;; else branch is untested
                      arg)))))))

(defmacro defhelper [name args body]
  `(defmacro ~name ~args
     `(let [~~@(interleave
                (for [arg args]
                  (list 'quote arg))
                (for [arg args]
                  (if (-> arg meta :compile)
                    ``(wt/compile ~~arg)
                    arg)))]
        ~~(list 'quote body))))

(defhelper link-to [^:compile title ^:compile url]
  (wt/compile [a {href ~url} ~title]))

(comment
  (defhelper link-to [^:compile title ^:compile url]
    ^:compile [a {href ~url} ~title]))

(t/deftest link-to-test
  (t/are [tmpl data html]
      (t/is (= html (wt/render-to-string (wt/compile tmpl) data)))

    [div ~(link-to "John" "/user/1")]
    nil
    "<div><a href=\"/user/1\">John</a></div>"

    [div ~(link-to (:name) (:url))]
    {:name "John" :url "/user/1"}
    "<div><a href=\"/user/1\">John</a></div>"))


;; ;; это больше похоже на макросы, чем на хэлперы
;; (deftmplmacro link-to
;;   ([title url]
;;    [a {href url} title])
;;   ([title url attrs]
;;    [a {href url
;;        attrs wtf??}]))

;; [div ~(link-to '(:name) '(:url))]
