(ns jtk-dvlp.xsd2xml.expand
  (:require
   [jtk-dvlp.xsd2xml.util :as util]
   [jtk-dvlp.xsd2xml.node :as node]))

(defn- expand-attribute
  [{:keys [attrs]}
   {{:keys [name]} :attrs :as node}]
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (attrs name)
            (attrs :default)
            (throw (ex-info (str "no attr '" name "' nor default") node)))

        expansion
        (expansion-provider original-provider node)]

    (-> node
        (assoc :content expansion)
        (update :attrs dissoc :type))))

(defn- expand-attribute-group
  [{:keys [attr-groups]}
   {{:keys [ref]} :attrs :as node}]

  (let [{expansion-provider :provider
         original-provider :parsed-provider :as x}
        (or (attr-groups ref)
            (attr-groups :default)
            (throw (ex-info (str "no attr-group '" ref "'") node)))

        expansion
        (expansion-provider original-provider node)]

    (:content expansion)))

(defn- expand-type
  [{:keys [types] {:keys [occurs]} :options}
   {{:keys [type minOccurs maxOccurs]} :attrs :as node}]

  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types type)
            (types :default)
            (throw (ex-info (str "no type '" type "' nor default") node)))

        min-occurs
        (or (util/parse-int minOccurs) 0)

        max-occurs
        (cond
          (= "unbounded" maxOccurs)
          occurs

          :else
          (or (util/parse-int maxOccurs) min-occurs))

        delta-occours
        (- max-occurs min-occurs)

        expansion-occurs
        (->> (max delta-occours 0)
             (+ min-occurs))]

    (->> #(expansion-provider original-provider node)
         (repeatedly expansion-occurs)
         (map #(-> node
                   (assoc :content %)
                   (update :attrs dissoc :type :minOccurs :maxOccurs))))))

(defn type-cycle
  [xsd-node]
  (when (node/attrs? #{:type} xsd-node)
    (get-in xsd-node [:attrs :type])))

(defn expand-xsd
  [{:keys [types] :as context} xsd-node]
  (cond
    (node/element-node? :element #{:type} xsd-node)
    (expand-type context xsd-node)

    (node/element-node? :attribute #{:type} xsd-node)
    (expand-attribute context xsd-node)

    (node/element-node? :attributeGroup #{:ref} xsd-node)
    (expand-attribute-group context xsd-node)

    (node/element-node? :simpleContent xsd-node)
    (update xsd-node :content conj ((-> types :default :provider)))

    :else xsd-node))
