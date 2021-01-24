(ns jtk-dvlp.xsd2xml.contract
  (:require
   [clojure.string :as str]
   [clojure.data.xml :as xml]
   [jtk-dvlp.xsd2xml.node :as node]))


(defn- attribute-or-content
  [{:keys [tag]}]
  (or (#{:attribute} tag)
      :content))

(defn- ->attr-kv-pair
  [{:keys [target-namespace-alias element-form-default]}
   {:keys [content] {:keys [name form]} :attrs}]
  (let [name
        (if (= "qualified" (or form element-form-default))
          (str target-namespace-alias ":" name)
          name)

        content
        (->> content
             (flatten)
             (filter node/text-node?)
             (str/join))]

    [name content]))

(defn- contract-element
  [{:keys [target-namespace-alias element-form-default] :as context}
   {:keys [content] {:keys [name form]} :attrs}]
  (let [name
        (if (= "qualified" (or form element-form-default))
          (str target-namespace-alias ":" name)
          name)

        {:keys [content]
         attrs :attribute}
        (->> content
             ;; WATCHOUT: xml-seq geht nicht, da durch das expand die Struktur genau genommen
             ;; unsauber ist, und seqs von seqs zustande kommen und somit keine gültigen
             ;; content seqs aus element vorliegen.
             (flatten)
             (group-by attribute-or-content))

        attrs
        (->> attrs
             (map (partial ->attr-kv-pair context))
             (into {}))]

    (xml/element name attrs content)))

(defn contract-xsd
  [context xsd-node]
  (cond
    (node/element-node? :element xsd-node)
    (contract-element context xsd-node)

    (node/element-node? :attribute xsd-node)
    xsd-node

    :else
    (:content xsd-node)))
