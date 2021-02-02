(ns jtk-dvlp.xsd2xml.expand
  (:require
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])

   [clojure.data.xml :as xml]
   [jtk-dvlp.xsd2xml.expand :as sut]))


(t/deftest expand-restriction-test
  (t/testing "complex type"
    (t/are [complex-type]
        (= (xml/element :restriction
             {}
             (xml/element complex-type
               {}
               (xml/element :element
                 {:name "a"
                  :type "string"
                  :maxOccurs "unbound"})
               (xml/element :element
                 {:name "b"
                  :type "string"})
               (xml/element :element
                 {:name "c"
                  :type "string"}))
             (xml/element :attribute
               {:name "a" :type "string"}))

           (sut/expand-restriction
            {:types
             {"base"
              {:provider
               (fn [& _]
                 (xml/element :complex-type
                   {}
                   (xml/element complex-type
                     {}
                     (xml/element :element
                       {:name "a"
                        :type "string"
                        :minOccurs 5})
                     (xml/element :element
                       {:name "b"
                        :type "string"})
                     (xml/element :element
                       {:name "c"
                        :type "string"
                        :maxOccurs 5}))))}}}

            (xml/element :restriction
              {:base "base"}
              (xml/element complex-type
                {}
                (xml/element :element
                  {:name "a"
                   :type "string"
                   :maxOccurs "unbound"})
                (xml/element :element
                  {:name "b"
                   :type "string"})
                (xml/element :element
                  {:name "c"
                   :type "string"}))
              (xml/element :attribute
                {:name "a" :type "string"}))))

      :sequence
      :choice
      :all))

  ,,,)
