(ns user
  (:require
   [nextjournal.clerk :as clerk]))

(comment
  (clerk/serve! {:browse? true :watch-paths ["notebooks"]})
  (clerk/show! "notebooks/readme.clj")

  (clerk/halt!)
  nil)
