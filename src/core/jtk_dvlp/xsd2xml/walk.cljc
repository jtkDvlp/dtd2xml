(ns jtk-dvlp.xsd2xml.walk
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]

   [jtk-dvlp.xsd2xml.node :as node]))


(def ^:dynamic *cycles* nil)
(def ^:dynamic *max-cycles* 3)

(defn cycle-safe-prewalk
  "Prewalk `form` applying `handle-fn` and stopping by cycling via cycle idenifier by `cycle-fn`."
  [handle-fn cycle-fn form]
  (binding [*cycles* (atom (if *cycles* @*cycles* {}))]
    (let [cycle
          (cycle-fn form)]

      (if (< (or (@*cycles* cycle) 0) *max-cycles*)
        (do
          (swap! *cycles* update cycle #(inc (or % 0)))
          (let [r (walk/walk (partial cycle-safe-prewalk handle-fn cycle-fn) identity (handle-fn form))]
            (swap! *cycles* update cycle dec)
            r))

        (handle-fn form)))))

(defn cycle-safe-prewalk-xml
  [handle-fn cycle-fn xml]
  (cycle-safe-prewalk
   (fn [xml]
     (cond
       (node/element-node? xml)
       (handle-fn xml)

       (node/text-node? xml)
       (str/trim xml)

       :else xml))
   cycle-fn
   xml))

(defn postwalk-xml
  [f xml]
  (walk/postwalk
   (fn [xml]
     (cond
       (node/element-node? xml)
       (f xml)

       (node/text-node? xml)
       (str/trim xml)

       :else xml))
   xml))
