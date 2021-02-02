(ns jtk-dvlp.xsd2xml.provider.primitive-types-test
  (:require
   #?(:clj [clojure.test :as t]
      :cljs [cljs.test :as t :include-macros true])

   [jtk-dvlp.xsd2xml.provider.primitive-types :as sut]))


(t/deftest string-provider-test
  (t/is (string? (sut/string-provider {} nil))
    "its a string")

  (t/is (#{"Eins" "Zwei" "Drei"}
         (sut/string-provider {:restrictions {:enumaration #{"Eins" "Zwei" "Drei"}}} nil))
    "enumaration")

  (t/is (try
          (sut/string-provider {:restrictions {:pattern ""}} nil)
          false
          (catch Exception _
            true))
    "pattern unsupported")

  (t/is (= 5 (count (sut/string-provider {:restrictions {:length 5}} nil)))
    "length")

  (t/is (let [length (count (sut/string-provider {:restrictions {:minLength 5 :maxLength 10}} nil))]
          (and (>= length 5) (<= length 10)))
    "min- / max-length")

  (t/is (>= (count (sut/string-provider {:restrictions {:minLength 5}} nil)) 5)
    "min-length")

  (t/is (<= (count (sut/string-provider {:restrictions {:maxLength 10}} nil)) 10)
    "max-length"))
