(ns jtk-dvlp.xsd2xml.core
  (:require
   [clojure.data.xml :as xml]
   [taoensso.timbre :as log]

   [jtk-dvlp.xsd2xml.node :as node]
   [jtk-dvlp.xsd2xml.walk :as walk]
   [jtk-dvlp.xsd2xml.collect :as collect]
   [jtk-dvlp.xsd2xml.expand :as expand]
   [jtk-dvlp.xsd2xml.contract :as contract]))


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

(defn- assoc-target-ns
  [{:keys [target-namespace target-namespace-alias]} node]
  (-> node
      ;; TODO: Warum funktioniert ein eigentlich richtiger ns nicht `(keyword "xmlns" target-namespace-alias)`
      ;; TODO: Müssten nich auch alle xmlns mit genommen werden, die im endxml verwendet werden?
      (assoc-in [:attrs :xmlns] target-namespace)
      (assoc-in [:attrs (str "xmlns:" target-namespace-alias)] target-namespace)))

(defn xsd->xml
  [{:keys [options types attrs attr-groups]} xsd-str]
  (let [xsd-nss
        (->> xsd-str
             (#(xml/parse-str % :namespace-aware false))
             (:attrs)
             (filter (comp #{"xmlns"} namespace first)))

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
        (collect/collect-types xsd-context xsd-nodes)

        xsd-attr-groups
        (collect/collect-attr-groups xsd-context xsd-nodes)

        xsd-context
        (assoc
         xsd-context
         :types (merge-with assoc-parsed-provider xsd-types types)
         :attr-groups (merge-with assoc-parsed-provider xsd-attr-groups attr-groups)
         :options (merge {:occurs 3} options))

        xml-root
        (->> xsd-root
             (:content)
             (filter element?)
             (first))]

    (binding [walk/*max-cycles*
              (or (:cycles options)
                  (:occurs options)
                  3)]

      (->> xml-root
           (walk/cycle-safe-xml-walk
            (partial expand/expand-xsd xsd-context)
            (partial contract/contract-xsd xsd-context)
            expand/type-cycle)
           (assoc-target-ns xsd-context)))
    ,,,))


(comment
  (log/set-level! :debug)

  (->> "test.xsd"
       (clojure.java.io/resource)
       (slurp)
       (xsd->xml {:options {:cycles 2 :occurs 3}
                  :types {:default {:provider (constantly ["Element Default-Wert"])}
                          "xs:integer" {:provider (fn [& _] [(rand-int 999999)])}}})
       ;; (clojure.pprint/pprint)
       (xml/indent-str)
       (spit "/tmp/test.xml")
       )

  ,,,)
