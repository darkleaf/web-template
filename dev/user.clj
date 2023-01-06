(ns user)


(comment
  (require '[nextjournal.clerk :as clerk])

  (clerk/serve! {:browse? true :watch-paths ["notebooks"]})
  (clerk/show! "notebooks/readme.clj")

  (clerk/halt!)

  (clerk/build! {:paths ["notebooks/readme.clj"]})
  nil)
