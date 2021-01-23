(ns jtk-dvlp.xsd2xml.expand
  (:require
   [jtk-dvlp.xsd2xml.util :as util]
   [jtk-dvlp.xsd2xml.node :as node]
   [jtk-dvlp.xsd2xml.walk :as walk]))

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

    (assoc node :content expansion)))

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

    (assoc node :content expansion)))

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
                   (update :attrs dissoc :type))))))

(defn expand-xsd
  [{:keys [types] :as context} xsd-root]

  (walk/cycle-safe-prewalk-xml
   (fn [node]
     (cond
       (node/element-node? :element #{:type} node)
       (expand-type context node)

       (node/element-node? :attribute #{:type} node)
       (expand-attribute context node)

       (node/element-node? :attributeGroup #{:ref} node)
       (expand-attribute-group context node)

       (node/element-node? :simpleContent node)
       (update node :content conj ((-> types :default :provider)))

       :else node))

   (fn [node]
     (when (node/attrs? #{:type} node)
       (get-in node [:attrs :type])))

   xsd-root))
