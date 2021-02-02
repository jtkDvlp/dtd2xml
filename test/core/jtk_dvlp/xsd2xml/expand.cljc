(ns jtk-dvlp.xsd2xml.expand
  (:require
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])

   [clojure.data.xml :as xml]
   [jtk-dvlp.xsd2xml.expand :as sut]))


(t/deftest expand-complex-restriction-test
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

         (sut/expand-complex-restriction
          {}
          {:provider
           (constantly
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
                   :maxOccurs 5}))))}
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
    :all)

  ,,,)

(t/deftest simple-elements->map-test
  (t/is (= {:a 1 :b 2 :c 3}
           (sut/simple-elements->map
            [(xml/element :a {:value 1})
             (xml/element :b {:value 2})
             (xml/element :c {:value 3})]))
    "without multiple values")

  (t/is (= {:a 1 :xs #{10 20 30} :c 3}
           (sut/simple-elements->map
            [(xml/element :a {:value 1})
             (xml/element :xs {:value 10})
             (xml/element :xs {:value 30})
             (xml/element :c {:value 3})
             (xml/element :xs {:value 20})]))
    "with multiple values"))

(t/deftest expand-simple-restriction-test
  (t/is (= (xml/element :restrictions
             {}
             {:length 1
              :minLength 2
              :maxLength 3})

           (sut/expand-simple-restriction
            {}
            {:provider
             (fn [context & _] [(:restrictions context)])}
            (xml/element :restrictions
              {:base "base"}
              (xml/element :length {:value 1})
              (xml/element :minLength {:value 2})
              (xml/element :maxLength {:value 3}))))
    "without enumaration")

  (t/is (= (xml/element :restrictions
             {}
             {:length 1
              :minLength 2
              :maxLength 3
              :enumeration #{:a :b :c}})

           (sut/expand-simple-restriction
            {}
            {:provider
             (fn [context & _] [(:restrictions context)])}
            (xml/element :restrictions
              {:base "base"}
              (xml/element :length {:value 1})
              (xml/element :minLength {:value 2})
              (xml/element :maxLength {:value 3})
              (xml/element :enumeration {:value :a})
              (xml/element :enumeration {:value :b})
              (xml/element :enumeration {:value :c}))))
    "with enumaration")

  ,,,)
