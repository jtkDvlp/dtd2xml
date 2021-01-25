(ns jtk-dvlp.xsd2xml.provider.special-types
  (:require
   [jtk-dvlp.xsd2xml.provider.primitive-types :as primitives]))


(defn token-provider
  [& _]
  (primitives/string-provider))

(def providers
  {"xs:token" {:name "xs:token" :provider token-provider}})
