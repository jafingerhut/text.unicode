(defproject com.fingerhutpress.text.unicode "0.1.0"
  :description "Some Clojure functions & macros for dealing with Unicode strings."
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :test-selectors {:default (fn [m] (and (not (:slow m))
                                         (not (:char-types m))
                                         (not (:normalized-forms m))
                                         (not (:property-names m))
                                         ))
                   :slow :slow
                   :char-types :char-types
                   :normalized-forms :normalized-forms
                   :property-names :property-names
                   :all (fn [m] true)})
