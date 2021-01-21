(ns jtk-dvlp.xsd2xml.core
  (:require
   [clojure.string :as str]
   [clojure.data.xml :as xml]
   [taoensso.timbre :as log]

   [jtk-dvlp.xsd2xml.node :as node]
   [jtk-dvlp.xsd2xml.util :as util]))


(def ^:private attr-group?
  (partial node/element-node-with-attrs? :attributeGroup #{:name}))

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

(defn- xsd->attr-groups
  "Collects all attribute-groups of `xsd-nodes` and returns a map of group-name
  and {:name group-name :provider f}."
  [context xsd-nodes]
  (->> xsd-nodes
       (filter attr-group?)
       (map (partial ->attr-group-provider context))
       (util/map-by :name)))

(def ^:private type?
  (some-fn
   (partial node/element-node-with-attrs? :complexType #{:name})
   (partial node/element-node-with-attrs? :simpleType #{:name})))

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

(defn- xsd->types
  "Collects all complex- and simple-types of `xsd-nodes` and returns a map of type-name
  and {:name type-name :type [:complex|:simple] :provider f}."
  [context xsd-nodes]
  (->> xsd-nodes
       (filter type?)
       (map (partial ->type-provider context))
       (util/map-by :name)))

(defn- assoc-parsed-provider
  [original custom]
  (assoc custom :parsed-provider original))

(def ^:private element?
  (partial node/element-node? :element))

(defn- ns->alias
  [nss ns]
  (->> nss
       (filter (comp #{ns} second))
       (ffirst)
       (name)))

(defn xsd->xml
  [{:keys [options types attrs attr-groups]} xsd]
  (let [xsd-str
        ;; TODO: CLJC!
        (slurp xsd)

        xsd-nss
        (node/collect-nss xsd-str)

        {{target-namespace :targetNamespace
          element-form-default :elementFormDefault} :attrs
         :as xsd-root}
        (xml/parse-str xsd-str)

        target-namespace-alias
        (ns->alias xsd-nss target-namespace)

        xsd-nodes
        (xml-seq xsd-root)

        xsd-context
        {:target-namespace-alias target-namespace-alias
         :target-namespace target-namespace
         :element-form-default element-form-default}

        xsd-types
        (xsd->types xsd-context xsd-nodes)

        xsd-attr-groups
        (xsd->attr-groups xsd-context xsd-nodes)

        xsd-context
        {:types (merge-with assoc-parsed-provider xsd-types types)
         :attrs attrs
         :attr-groups (merge-with assoc-parsed-provider xsd-attr-groups attr-groups)
         :options (merge {:occurs 3} options)}

        xml-root
        (->> xsd-root
             (:content)
             (filter element?)
             (first)
             (#(assoc-in % [:attrs (keyword "xmlns" target-namespace-alias)] target-namespace)))]


    xml-root
    ,,,))




(comment
  (->> "test.xsd"
       (clojure.java.io/resource)
       (xsd->xml {})
       (clojure.pprint/pprint)
       )

  ,,,)
