(ns jtk-dvlp.xsd2xml.node
  (:require
   [clojure.set :refer [subset?]]
   [clojure.data.xml :as xml]))


(defn attrs?
  [attrs xml]
  (->> xml (:attrs) (keys)
       (into #{})
       (subset? attrs)))

(defn element-node?
  "Is `xml` an element-node structured like following {:tag _, :attrs _, :content _}.
  Given `tag` also test value of `:tag`?"
  ([xml]
   ((every-pred map? :tag :attrs :content) xml))

  ([tag xml]
   ((every-pred
     element-node?
     (comp #{tag} :tag))
    xml))

  ([tag attrs xml]
   ((every-pred
     (partial element-node? tag)
     (partial attrs? attrs))
    xml)))

(def text-node?
  "Is `xml` an text-node (string)?"
  (some-fn string? number?))

(def node?
  "Is `xml` `element-node` or `text-node`?"
  (some-fn element-node? text-node?))

(def container?
  "Is `xml` no node but container like structure [] | '()?"
  (some-fn list? vector? seq?))
