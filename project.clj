(defproject com.fingerhutpress.text.unicode "1.0.0-SNAPSHOT"
  :description "Some Clojure functions & macros for dealing with Unicode strings."
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :test-selectors {:default (fn [m] (and (not (:slow m))
                                         (not (:write-char-types-to-file m))
                                         (not (:write-nfc-nfd-to-file m))
                                         (not (:test-unicode-property-names m))
                                         ))
                   :slow :slow
                   :write-char-types-to-file :write-char-types-to-file
                   :write-nfc-nfd-to-file :write-nfc-nfd-to-file
                   :test-unicode-property-names :test-unicode-property-names
                   :all (fn [m] true)})
