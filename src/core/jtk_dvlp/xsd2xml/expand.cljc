(ns jtk-dvlp.xsd2xml.expand
  (:require
   [clojure.string :as str]
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
        ;; TODO: Wie wird expandet und was wird in content gepackt, gerade ziehen!
        (assoc :content [expansion])
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

(defn- separate-element-container
  [content]
  (-> #(if (#{:sequence :choice :all} (:tag %))
         :element-container
         :rest-tags)
      (group-by content)))

(defn- expand-extension
  [{:keys [types] :as context}
   {{:keys [base]} :attrs
    extension-content :content :as node}]

  (log/trace "expand-extension" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types base)
            (throw (ex-info (str "no type '" base "'") node)))

        {base-content :content}
        (expansion-provider context original-provider node)

        {[base-element-container] :element-container
         base-rest-tags :rest-tags}
        (separate-element-container base-content)

        {[{extension-elements :content}] :element-container
         extension-rest-tags :rest-tags}
        (separate-element-container extension-content)

        rest-tags
        (concat base-rest-tags extension-rest-tags)

        element-container
        (update base-element-container :content concat extension-elements)

        content
        (-> (cons element-container rest-tags))]

    (-> node
        (assoc :content content)
        (update :attrs dissoc :base))))

(defn- merge-elements
  [by with & xs]
  (->> xs
       (apply concat)
       ;; WATCHOUT: Unsecure ordering
       (group-by by)
       (vals)
       (map with)))

(defn- expand-complex-restriction
  [context
   {expansion-provider :provider
    original-provider :parsed-provider}
   {restriction-content :content :as node}]

  (log/trace "expand-complex-restriction" node)
  (let [{base-content :content}
        (expansion-provider context original-provider node)

        {[{base-elements :content :as base-element-container}] :element-container
         base-rest-tags :rest-tags}
        (separate-element-container base-content)

        {[{extension-elements :content}] :element-container
         extension-rest-tags :rest-tags}
        (separate-element-container restriction-content)

        rest-tags
        (merge-elements
         (comp :name :attrs) peek
         base-rest-tags extension-rest-tags)

        element-container
        (->> extension-elements
             (merge-elements (comp :name :attrs) peek base-elements)
             (assoc base-element-container :content))

        content
        (-> (cons element-container rest-tags))]

    (-> node
        (assoc :content content)
        (update :attrs dissoc :base))))

(defn- simple-elements->map
  [elements]
  (reduce
   (fn [m {:keys [tag] {:keys [value]} :attrs}]
     (if (contains? m tag)
       (update m tag #(conj (if (set? %1) %1 #{%1}) %2) value)
       (assoc m tag value)))
   {} elements))

(defn- expand-simple-restriction
  [context
   {expansion-provider :provider
    original-provider :parsed-provider}
   {restriction-content :content :as node}]

  (log/trace "expand-simple-restriction" node)
  (let [restrictions
        (simple-elements->map restriction-content)

        ;; TODO: expansion provider vereinheitlichen
        content
        (-> context
            (assoc :original-provider original-provider
                   :restrictions restrictions)
            (expansion-provider node))]

    (-> node
        (assoc :content content)
        (update :attrs dissoc :base))))

(defn- expand-restriction
  [{:keys [types] :as context}
   {{:keys [base]} :attrs :as node}]

  (log/trace "expand-restriction" node)
  (let [base
        (or (types base)
            (throw (ex-info (str "no type '" base "'") node)))]

    (case (:type base)
      :complex
      (expand-complex-restriction context base node)

      :simple
      (expand-simple-restriction context base node)

      (throw (ex-info (str "base type '" type "' not supported") node)))))

(defn- expand-list
  [{:keys [types] {:keys [occurs]} :options :as context}
   {{:keys [itemType]} :attrs :as node}]

  (log/trace "expand-list" node)
  (let [{expansion-provider :provider
         original-provider :parsed-provider}
        (or (types itemType)
            (throw (ex-info (str "no type '" itemType "'") node)))

        extension-content
        (->> #(expansion-provider context original-provider node)
             (repeatedly occurs)
             (into #{})
             (str/join " "))]

    (-> node
        (update :content conj extension-content)
        (update :attrs dissoc :itemType))))

(defn- expand-union
  [{:keys [types] :as context}
   {{:keys [memberTypes]} :attrs :as node}]

  (log/trace "expand-union" node)
  (let [memberType
        (-> memberTypes
            (str/split #"\s+")
            (rand-nth))

        {expansion-provider :provider
         original-provider :parsed-provider}
        (or (types memberType)
            (throw (ex-info (str "no type '" memberType "'") node)))

        extension-content
        (expansion-provider context original-provider node)]

    (-> node
        (update :content conj extension-content)
        (update :attrs dissoc :memberType))))

(defn- expand-element
  [{:keys [types] {:keys [occurs]} :options :as context}
   {{:keys [type minOccurs maxOccurs]} :attrs :as node}]

  (log/trace "expand-element" node)
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

    (log/trace "expand-element" "repeat" expansion-occurs "times")
    (->> #(expansion-provider context original-provider node)
         (repeatedly expansion-occurs)
         (map #(-> node
                   (assoc :content %)
                   (update :attrs dissoc :type :minOccurs :maxOccurs))))))

(defn- expand-sequence
  [_context {:keys [content] :as node}]

  (log/trace "expand-sequence" node)
  (->> content
       (filter (partial node/element-node? :element #{:type}))
       (assoc node :content)))

(defn- expand-choice
  [_context {:keys [content] :as node}]

  (log/trace "expand-choice" node)
  (->> content
       (filter (partial node/element-node? :element #{:type}))
       (rand-nth)
       (vector)
       (assoc node :content)))

(defn- expand-all-seq
  [_context {:keys [content] :as node}]

  (log/trace "expand-all-seq" node)
  (->> content
       (filter (partial node/element-node? :element #{:type}))
       (shuffle)
       (assoc node :content)))

(defn type-cycle
  [xsd-node]
  (get-in xsd-node [:attrs :type]))

(defn expand-xsd
  [context xsd-node]
  (log/trace "expand-xsd" xsd-node)
  (cond
    (node/element-node? :element #{:type} xsd-node)
    (expand-element context xsd-node)

    (node/element-node? :sequence xsd-node)
    (expand-sequence context xsd-node)

    (node/element-node? :choice xsd-node)
    (expand-choice context xsd-node)

    (node/element-node? :all xsd-node)
    (expand-all-seq context xsd-node)

    (node/element-node? :list #{:itemType} xsd-node)
    (expand-list context xsd-node)

    (node/element-node? :union #{:memberTypes} xsd-node)
    (expand-union context xsd-node)

    (node/element-node? :extension #{:base} xsd-node)
    (expand-extension context xsd-node)

    (node/element-node? :restriction #{:base} xsd-node)
    (expand-restriction context xsd-node)

    (node/element-node? :attribute #{:type} xsd-node)
    (expand-attribute context xsd-node)

    (node/element-node? :attributeGroup #{:ref} xsd-node)
    (expand-attribute-group context xsd-node)

    :else xsd-node))
