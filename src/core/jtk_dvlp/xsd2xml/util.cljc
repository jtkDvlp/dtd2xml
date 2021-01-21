(ns jtk-dvlp.xsd2xml.util)


(defn parse-int
  "Parses `x` to `int` returning `nil` on error."
  [x]
  #?(:clj
     (try
       (if-not (integer? x)
         (Integer/parseInt x)
         x)
       (catch Exception _e nil))

     :cljs
     (try
       (if-not (integer? x)
         (js/parseInt x)
         x)
       (catch :default _e nil))))

(defn map-by
  [f coll]
  (->> coll
       (map (juxt f identity))
       (into {})))
