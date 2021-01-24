(ns jtk-dvlp.xsd2xml.contract
  (:require
   [clojure.string :as str]
   [clojure.data.xml :as xml]
   [jtk-dvlp.xsd2xml.node :as node]))


(defn- attribute-or-content
  [{:keys [tag]}]
  (or (#{:attribute} tag)
      :content))

(def ^:private ->attr-kv-pair
  (juxt
   (comp :name :attrs)
   (comp str/join :content)))

(defn- contract-element
  [_context xsd-node]
  (let [{:keys [content]
         {:keys [name]} :attrs}
        xsd-node

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
             (map ->attr-kv-pair)
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
