(defproject jtk-dvlp/xsdd2xml "0.0.0-SNAPSHOT"
  :description
  "Generates XML via XSD rules."

  :url
  "https://github.com/jtkDvlp/xsd2xml"

  :license
  {:name
   "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"

   :url
   "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies
  [[org.clojure/clojure "1.10.0"]
   [org.clojure/data.xml "0.0.8"]
   [com.taoensso/timbre "5.1.0"]
   [clj-time "0.15.2"]]

  :source-paths
  ["src/core"]

  :target-path
  "target/%s"

  :clean-targets
  ^{:protect false}
  [:target-path]

  :profiles
  {:cli
   {:main
    jtk-dvlp.xsd2xml.cli

    :dependencies
    [[org.clojure/tools.cli "1.0.194"]]

    :source-paths
    ["src/cli"]}

   :dev
   {:resource-paths
    ["resources/dev"]

    :repl-options
    {:init-ns
     jtk-dvlp.xsd2xml.core}}

   ,,,})
