(defproject jtk-dvlp/dtd2xml "0.0.0-SNAPSHOT"
  :description
  "CI to generate XML-Test-Data via DTD rules."

  :url
  "https://github.com/jtkDvlp/dtd2xml"

  :license
  {:name
   "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"

   :url
   "https://www.eclipse.org/legal/epl-2.0/"}

  :main
  jtk-dvlp.dtd2xml.main

  :source-paths
  ["src"]

  :target-path
  "target"

  :clean-targets
  ^{:protect false}
  [:target-path]

  :profiles
  {:provided
   {:dependencies
    [[org.clojure/clojure "1.10.0"]]}

   :repl
   {:repl-options
    {:init-ns
     user}}

   ,,,})
