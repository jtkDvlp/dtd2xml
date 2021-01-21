(ns jtk-dvlp.xsd2xml.node
  (:require
   [clojure.set :refer [subset?]]
   [clojure.data.xml :as xml]))


(defn element-node?
  "Is `xml` an element-node structured like following {:tag _, :attrs _, :content _}.
  Given `tag-name` also test value of `:tag`?"
  ([xml]
   ((every-pred map? :tag :attrs :content) xml))

  ([tag-name xml]
   ((every-pred
     element-node?
     (comp #{tag-name} :tag))
    xml)))

(defn element-node-with-attrs?
  "Is `xml` element-node with tag `tag-name` and has set of attributes `attrs`?"
  ([attrs xml]
   (->> xml (:attrs) (keys)
        (into #{})
        (subset? attrs)))

  ([tag-name attrs xml]
   ((every-pred
     (partial element-node? tag-name)
     (comp (partial subset? attrs)
           (partial into #{})
           keys :attrs))
    xml)))

(def text-node?
  "Is `xml` an text-node (string)?"
  string?)

(def node?
  "Is `xml` `element-node` or `text-node`?"
  (some-fn element-node? text-node?))

(def container?
  "Is `xml` no node but container like structure [] | '()?"
  (some-fn list? vector? seq?))

(defn collect-nss
  [source]
  (->> source
       (#(xml/parse-str % :namespace-aware false))
       (xml-seq)
       (map :attrs)
       (reduce merge {})
       (filter (comp #{"xmlns"} namespace first))))
