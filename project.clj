(defproject com.fingerhutpress.text.unicode "0.1.0"
  :description "Some Clojure functions & macros for dealing with Unicode strings."
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :test-selectors {:default (fn [m] (and (not (:slow m))
                                         (not (:write-char-types-to-file m))
                                         (not (:write-normalized-forms-to-file m))
                                         (not (:test-unicode-property-names m))
                                         ))
                   :slow :slow
                   :write-char-types-to-file :write-char-types-to-file
                   :write-normalized-forms-to-file :write-normalized-forms-to-file
                   :test-unicode-property-names :test-unicode-property-names
                   :all (fn [m] true)})
