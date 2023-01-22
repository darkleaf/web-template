(ns user)

(comment
  (require '[criterium.core :as c])

  (let [m {:a 1}]
    (c/quick-bench
     (get m :a))) ; Execution time mean : 4,475954 ns

  (let [m {'a 1}]
    (c/quick-bench
     (get m 'a)))) ; Execution time mean : 7,571531 ns

(comment
  (require '[nextjournal.clerk :as clerk])

  (clerk/serve! {:browse? true :watch-paths ["notebooks"]})
  (clerk/show! "notebooks/readme.clj")

  (clerk/halt!)

  (clerk/build! {:paths ["notebooks/readme.clj"]})
  nil)
