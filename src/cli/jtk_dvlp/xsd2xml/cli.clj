(ns jtk-dvlp.xsd2xml.cli
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.cli :refer [parse-opts]]

   [taoensso.timbre :as log])

  (:import
   [clojure.lang ExceptionInfo]))


(defn generate
  [{:keys [xsd xml] :as opts}]
  (log/debug "generate" opts)
  (when-not xsd (throw (ex-info "missing xsd" {:code [:missing :xsd] :xsd xsd})))
  (when-not xml (throw (ex-info "missing xml" {:code [:missing :xml] :xml xml})))

  (println "got all options :+1"))

(def ^:private options
  [["-v" nil "Verbosity level"
    :id :verbosity
    :default 0
    :update-fn dec]])

(def ^:private actions
  [["generate" #'generate "Generates XML by XSD"
    [["-i" "--xsd FILE" "XSD file"
      :validate [#(.exists (io/file %)) "File not found"]]

     ["-o" "--xml FILE" "XML file"
      :validate [#(not (.exists (io/file %))) "File already exists"]]]]])

(defn- print-options
  [options-summary action]
  (->> [(str "Usage: xsd2xml [options] " action " [options]")
        ""
        "Options:"
        options-summary
        ""]
       (str/join \newline)
       (println)))

(defn- print-usage
  [options-summary actions-summary]
  (->> ["Generates XML via XSD rules."
        ""
        "Usage: xsd2xml [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        actions-summary
        "  help\tShows help / usage"
        "  version\tShows version"
        ""]
       (str/join \newline)
       (println)))

(defn- print-version
  []
  (-> "xsd2xml.version"
      (System/getProperty)
      (println)))

(defn- print-errors
  [errors]
  (println "ERROR: "
           (str/join \newline errors)
           \newline))


;; TODO: Cleanup
(defn -main
  [& args]
  (let [actions-summary
        (->> actions
             (map (fn [[action _ desc]] (str "  " action "\t" desc)))
             (str/join \newline))

        {[action & arguments] :arguments
         options-summary :summary
         {:keys [verbosity] :as options} :options
         :keys [errors]}
        (parse-opts args options :in-order true)

        [_ action-fn _ action-options]
        (->> actions
             (filter (comp #{action} first))
             (first))]

    (->> verbosity
         (+ 3)
         (max 0)
         (nth log/ordered-levels)
         (log/set-level!))

    (try
      (cond
        errors
        (do (print-errors errors)
            (print-usage options-summary actions-summary))

        (= action "help")
        (print-usage options-summary actions-summary)

        (= action "version")
        (print-version)

        (nil? action-fn)
        (do (print-errors ["action not found"])
            (print-usage options-summary actions-summary))

        :else
        (let [{action-options-summary :summary
               action-options :options
               action-errors :errors
               [argument] :arguments}
              (parse-opts arguments action-options)]

          (cond
            action-errors
            (do (print-errors action-errors)
                (print-options action-options-summary action))

            (= "help" argument)
            (print-options action-options-summary action)

            :else
            (try
              (action-fn (merge options action-options))
              (catch ExceptionInfo e
                (print-errors [(ex-message e)])
                (print-options action-options-summary action)
                (System/exit 1)
                ,,,)))))

      (catch Throwable e
        (log/error e)
        (System/exit 1)
        ,,,))))
