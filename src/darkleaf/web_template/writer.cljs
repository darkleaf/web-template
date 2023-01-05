(ns darkleaf.web-template.writer
  (:import
   (goog.string StringBuffer)))

(defn- escape [s]
  (.. s
      (replace "&"  "&amp;")
      (replace "<"  "&lt;")
      (replace ">"  "&gt;")
      (replace "\"" "&quot;")
      (replace "'"  "&apos;")))

(defn append-raw [w str]
  (.append w str))

(defn append [w str]
  (->> str
       escape
       (.append w)))

(defn write-to-string [callback]
  (let [sw (StringBuffer.)]
    (callback sw)
    (.toString sw)))
