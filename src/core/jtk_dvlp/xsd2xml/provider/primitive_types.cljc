(ns jtk-dvlp.xsd2xml.provider.primitive-types
  (:require
   [clojure.string :as str]
   [clj-time.core :as time]
   [clj-time.format :as time-format]))


(def ^:private lorem-ipsum
  (-> "Lorem ipsum dolor sit amet consetetur sadipscing elitr sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat sed diam voluptua At vero eos et accusam et justo duo dolores et ea rebum Stet clita kasd gubergren no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet consetetur sadipscing elitr sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat sed diam voluptua At vero eos et accusam et justo duo dolores et ea rebum Stet clita kasd gubergren no sea takimata sanctus est Lorem ipsum dolor sit amet"
      (str/split #"\s")))

(defn string-provider
  [& _]
  (->> (random-sample 0.01 lorem-ipsum)
       (cons (rand-nth lorem-ipsum))
       (str/join " ")
       (vector)))

(defn decimal-provider
  [& _]
  [(* (rand-int 9999) (rand))])

(defn integer-provider
  [& _]
  [(rand-int 9999)])

(def float-provider
  decimal-provider)

(defn boolean-provider
  [& _]
  [(= 1 (rand-int 2))])

(def ^:private date-formatter
  (time-format/formatter "yyyy-MM-dd"))

(defn date-provider
  [& _]
  [(-> (time/now)
       (time/plus (time/days (rand (* 5 365))))
       (time/minus (time/days (rand (* 5 365))))
       (#(time-format/unparse date-formatter %)))])

(def ^:private time-formatter
  (time-format/formatter "HH:mm:ss"))

(defn time-provider
  [& _]
  [(-> (time/now)
       (time/plus (time/seconds (rand (* 24 60 60))))
       (time/minus (time/seconds (rand (* 24 60 60))))
       (#(time-format/unparse time-formatter %)))])

(def providers
  {"xs:string" {:name "xs:string" :provider string-provider}
   "xs:decimal" {:name "xs:decimal" :provider decimal-provider}
   "xs:integer" {:name "xs:integer" :provider integer-provider}
   "xs:float" {:name "xs:float" :provider float-provider}
   "xs:boolean" {:name "xs:boolean" :provider boolean-provider}
   "xs:date" {:name "xs:date" :provider date-provider}
   "xs:time" {:name "xs:time" :provider time-provider}})
