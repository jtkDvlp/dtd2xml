(ns jtk-dvlp.xsd2xml.walk
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]
   [taoensso.timbre :as log]

   [jtk-dvlp.xsd2xml.node :as node]))


(def ^:dynamic *cycles* nil)
(def ^:dynamic *max-cycles* 3)

(defn cycle-safe-walk
  "Walk `form` applying `pre-fn` and stopping by cycling via cycle idenifier by `cycle-fn` and `post-fn`."
  [pre-fn post-fn cycle-fn form]
  (binding [*cycles* (atom (if *cycles* @*cycles* {}))]
    (log/trace "cycle-safe-prewalk" "walk for form with cycles" form @*cycles*)
    (let [cycle
          (cycle-fn form)

          cycle-depth
          (or (@*cycles* cycle) 1)]

      (cond
        (nil? cycle)
        (do (log/trace "cycle-safe-prewalk" "no cycle")
            (walk/walk (partial cycle-safe-walk pre-fn post-fn cycle-fn) post-fn (pre-fn form)))

        (<= cycle-depth *max-cycles*)
        (do
          (log/trace "cycle-safe-prewalk" "with cycle" cycle "and depth" cycle-depth)
          (swap! *cycles* update cycle #(inc (or % 0)))
          (let [r (walk/walk (partial cycle-safe-walk pre-fn post-fn cycle-fn) post-fn (pre-fn form))]
            (swap! *cycles* update cycle dec)
            r))

        :else
        (do
          (log/info "cycle-safe-prewalk" "stop cycle for" cycle (dec cycle-depth))
          nil)))))

(defn- handle-xml
  [handle-fn xml]
  (cond
    (node/element-node? xml)
    (handle-fn xml)

    (node/text-node? xml)
    xml

    :else xml))

(defn cycle-safe-xml-walk
  [pre-fn post-fn cycle-fn xml]
  (cycle-safe-walk
   (partial handle-xml pre-fn)
   (partial handle-xml post-fn)
   cycle-fn
   xml))
