(defproject com.fingerhutpress.text.unicode "1.0.0-SNAPSHOT"
  :description "Some Clojure functions & macros for dealing with Unicode strings."
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :test-selectors {:default (fn [m] (not (:slow m)))
                   :slow :slow
                   :all (fn [m] true)})
