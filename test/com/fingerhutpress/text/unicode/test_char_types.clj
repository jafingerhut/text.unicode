(ns com.fingerhutpress.text.unicode.test-char-types
  (:use [clojure.test])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :as p]
            [com.fingerhutpress.text.unicode :as u]
            [com.fingerhutpress.text.unicode.test-helpers :as h]))

(set! *warn-on-reflection* true)


;; Note: deftest write-char-types-to-file isn't really so much a unit
;; test as a mechanism of dumping info that the JVM stores about each
;; character out to a file.  It is human readable, but perhaps better
;; used for post-processing and/or comparing against Unicode data
;; files, or against the output of a similar program in another
;; language like Perl to see how their character info compares to each
;; other.


;; The first element of each pair is the abbreviated "general
;; category" from the Unicode specification, according to the Java
;; documentation for class java.lang.Character here:

;; http://docs.oracle.com/javase/1.5.0/docs/api/java/lang/Character.html

;; The third element is the name of a static member of the class
;; Character, which is a Java-specific numeric value returned by
;; method getType(int codePoint).

;; The second element is my own made-up abbreviation of the third
;; element, for a smaller output file.

(def unicode-category-raw-data [
                   [ "C"  nil      nil ]
                   [ "Cn" "UNASS" "UNASSIGNED" ]
                   [ "L"  nil      nil ]
                   [ "Lu" "UCLET" "UPPERCASE_LETTER" ]
                   [ "Ll" "LCLET" "LOWERCASE_LETTER" ]
                   [ "Lt" "TCLET" "TITLECASE_LETTER" ]
                   [ "Lm" "MOLET" "MODIFIER_LETTER" ]
                   [ "Lo" "OTLET" "OTHER_LETTER" ]
                   [ "M"  nil      nil ]
                   [ "Mn" "NSMRK" "NON_SPACING_MARK" ]
                   [ "Me" "ENMRK" "ENCLOSING_MARK" ]
                   [ "Mc" "CSMRK" "COMBINING_SPACING_MARK" ]
                   [ "Nd" "DDNUM" "DECIMAL_DIGIT_NUMBER" ]
                   [ "N"  nil      nil ]
                   [ "Nl" "LENUM" "LETTER_NUMBER" ]
                   [ "No" "OTNUM" "OTHER_NUMBER" ]
                   [ "Z"  nil      nil ]
                   [ "Zs" "SPSEP" "SPACE_SEPARATOR" ]
                   [ "Zl" "LNSEP" "LINE_SEPARATOR" ]
                   [ "Zp" "PGSEP" "PARAGRAPH_SEPARATOR" ]
                   [ "Cc" "CNTRL" "CONTROL" ]
                   [ "Cf" "FORMT" "FORMAT" ]
                   [ "Co" "PRIVU" "PRIVATE_USE" ]
                   [ "Cs" "SURRO" "SURROGATE" ]
                   [ "P"  nil      nil ]
                   [ "Pd" "DSPNC" "DASH_PUNCTUATION" ]
                   [ "Ps" "STPNC" "START_PUNCTUATION" ]
                   [ "Pe" "ENPNC" "END_PUNCTUATION" ]
                   [ "Pc" "CNPNC" "CONNECTOR_PUNCTUATION" ]
                   [ "Po" "OTPNC" "OTHER_PUNCTUATION" ]
                   [ "S"  nil      nil ]
                   [ "Sm" "MASYM" "MATH_SYMBOL" ]
                   [ "Sc" "CUSYM" "CURRENCY_SYMBOL" ]
                   [ "Sk" "MOSYM" "MODIFIER_SYMBOL" ]
                   [ "So" "OTSYM" "OTHER_SYMBOL" ]
                   [ "Pi" "IQPNC" "INITIAL_QUOTE_PUNCTUATION" ]
                   [ "Pf" "FQPNC" "FINAL_QUOTE_PUNCTUATION" ]
                   ])


(def unicode-general-cats-sorted
     (sort (map first unicode-category-raw-data)))


(def unicode-category-info
     (reduce (fn [m [unicode-general-cat short-java-name java-name]]
               (let [n (if java-name
                         (eval (read-string (str "Character/" java-name)))
                         -1)]
                 (assoc m unicode-general-cat
                        {
                         :unicode-general-category unicode-general-cat,
                         :java-char-type-name java-name
                         :short-java-char-type-name short-java-name
                         :java-enum-int-val n
                         :re-pattern
                         (re-pattern (str "^\\p{" unicode-general-cat "}$"))
                         })))
             {} unicode-category-raw-data))


(def num-to-cat-info
     (reduce (fn [m [k v]]
               (assoc m (:java-enum-int-val v) v))
             {} unicode-category-info))



(defn print-cp-info-line [cp-info]
;  (printf "print-cp-info-line: cp-info=")
;  (p/pprint cp-info)
;  (flush)
  (printf "U+%06x %2d %-5s %s\n" (cp-info :cp)
          (cp-info :java-enum-int-val)
          (get-in num-to-cat-info [(cp-info :java-enum-int-val)
                                   :short-java-char-type-name])
          (cp-info :matching-cat-list)))


(deftest ^:char-types write-char-types-to-file
  (let [fname "char-types.txt"]
    (println (format "Writing file '%s' ..." fname))
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (h/print-interesting-jvm-version-properties)
        (printf "\n")
        (let [similar-char-groups
              (->> (h/all-codepoints)
                   ;; Create a map for each codepoint with some info
                   (map (fn [i] {:cp i
                                 :java-enum-int-val (Character/getType (int i))
                                 :str (u/chr i)}))
;;                   ;; Skip over code points that are private or unassigned.
;;                   (remove (fn [m]
;;                             (#{ Character/UNASSIGNED
;;                                Character/PRIVATE_USE } (:java-enum-int-val m))))
                   ;; To each map, add key :matching-cat-list
                   (map (fn [m]
                          (assoc m
                            :matching-cat-list
                            (str/join " " (filter 
                                           #(re-find (get-in unicode-category-info [% :re-pattern])
                                                     (:str m))
                                           unicode-general-cats-sorted))
                            )))

                   ;; To each map, add key :uniq-key for grouping things with
                   ;; identical :java-num-int-val's and :matching-cat-list's.
;;                   (map (fn [m] (assoc m :uniq-key
;;                                       [(:java-num-int-val m) (:matching-cat-list m)])))
;;                   (partition-by :uniq-key)
                   )]
          (doseq [cp-info similar-char-groups]
            (print-cp-info-line cp-info))
;;          (doseq [g similar-char-groups]
;;            ;; Print only the first and last codepoints of the range
;;            ;; that have the same results, to make the output
;;            ;; significantly shorter.
;;            (let [n (count g)]
;;;              (printf "Group size=%d\n" n)
;;              (print-cp-info-line (first g))
;;              (when (>= n 3)
;;                (printf "   ... %d others with same type and matching results removed ...\n" (- n 2)))
;;              (when (>= n 2)
;;                (print-cp-info-line (last g)))))
          (flush)
          )))))
