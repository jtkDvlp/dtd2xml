(ns jtk-dvlp.xsd2xml.collect
  (:require
   [clojure.string :as str]

   [jtk-dvlp.xsd2xml.util :as util]
   [jtk-dvlp.xsd2xml.node :as node]))


(def ^:private attr-group?
  (partial node/element-node? :attributeGroup #{:name}))

(defn- ->attr-group-provider
  [{:keys [target-namespace-alias element-form-default]}
   {{type-name :name :keys [form]} :attrs :as node}]

  (let [qualified?
        (#{"qualified"} (or form element-form-default))

        name
        (if qualified?
          (str target-namespace-alias ":" type-name)
          type-name)]

    {:name name
     :provider (constantly node)}))

(defn collect-attr-groups
  "Collects all attribute-groups of `xsd-nodes` and returns a map of group-name
  and {:name group-name :provider f}."
  [context xsd-nodes]
  (->> xsd-nodes
       (filter attr-group?)
       (map (partial ->attr-group-provider context))
       (util/map-by :name)))

(def ^:private type?
  (some-fn
   (partial node/element-node? :complexType #{:name})
   (partial node/element-node? :simpleType #{:name})))

(defn- ->type-provider
  [{:keys [target-namespace-alias element-form-default]}
   {tag-name :tag {type-name :name :keys [form]} :attrs :as node}]

  (let [qualified?
        (#{"qualified"} (or form element-form-default))

        name'
        (if qualified?
          (str target-namespace-alias ":" type-name)
          type-name)]

    {:name name'
     :type (-> tag-name (name) (str/replace "Type" "") (keyword))
     :provider (constantly node)}))

(defn collect-types
  "Collects all complex- and simple-types of `xsd-nodes` and returns a map of type-name
  and {:name type-name :type [:complex|:simple] :provider f}."
  [context xsd-nodes]
  (->> xsd-nodes
       (filter type?)
       (map (partial ->type-provider context))
       (util/map-by :name)))
