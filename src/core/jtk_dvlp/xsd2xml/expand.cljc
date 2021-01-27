(ns jtk-dvlp.xsd2xml.expand
  (:require
   [taoensso.timbre :as log]

   [jtk-dvlp.xsd2xml.util :as util]
   [jtk-dvlp.xsd2xml.node :as node]))

(defn- expand-attribute
  [{:keys [types]}
   {{:keys [type]} :attrs :as node}]

  (log/trace "expand-attribute" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types type)
            (log/spy :debug (str "expand-attribute default type instead of " type) (types :default))
            (throw (ex-info (str "no type '" type "' nor default") node)))

        expansion
        (expansion-provider original-provider node)]

    (-> node
        (assoc :content expansion)
        (update :attrs dissoc :type))))

(defn- expand-attribute-group
  [{:keys [attr-groups]}
   {{type :ref} :attrs :as node}]

  (log/trace "expand-attribute-group" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (attr-groups type)
            (log/spy :debug (str "expand-attribute-group default type instead of " type) (attr-groups :default))
            (throw (ex-info (str "no attr-group '" type "'") node)))

        expansion
        (expansion-provider original-provider node)]

    (:content expansion)))

(defn- expand-type-extension
  [{:keys [types] :as context}
   {{:keys [base]} :attrs :as node}]

  (log/trace "expand-type-extension" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types base)
            (throw (ex-info (str "no type '" base "'") node)))

        extension-content
        (expansion-provider context original-provider node)]

    (-> node
        (update :content conj extension-content)
        (update :attrs dissoc :base))))

(defn- expand-type
  [{:keys [types] {:keys [occurs]} :options :as context}
   {{:keys [type minOccurs maxOccurs]} :attrs :as node}]

  (log/trace "expand-type" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types type)
            (log/spy :debug (str "expand-type default type instead of " type) (types :default))
            (throw (ex-info (str "no type '" type "' nor default") node)))

        min-occurs
        (or (util/parse-int minOccurs) 1)

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


    (log/trace "expand-type" "repeat" expansion-occurs "times")
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
  [context xsd-node]
  (log/trace "expand-xsd" xsd-node)
  (cond
    (node/element-node? :element #{:type} xsd-node)
    (expand-type context xsd-node)

    (node/element-node? :extension #{:base} xsd-node)
    (expand-type-extension context xsd-node)

    (node/element-node? :attribute #{:type} xsd-node)
    (expand-attribute context xsd-node)

    (node/element-node? :attributeGroup #{:ref} xsd-node)
    (expand-attribute-group context xsd-node)

    :else xsd-node))
