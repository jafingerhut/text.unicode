(ns com.fingerhutpress.text.unicode.test-helpers
  (:require [clojure.string :as str]
            [com.fingerhutpress.text.unicode :as u]))


(defn print-interesting-jvm-version-properties []
  (let [p (System/getProperties)]
    (doseq [prop-name [ "java.class.version"
                        "java.runtime.name"
                        "java.runtime.version"
                        "java.specification.name"
                        "java.specification.vendor"
                        "java.specification.version"
                        "java.vendor"
                        "java.vendor.url"
                        "java.vendor.url.bug"
                        "java.version"
                        "java.vm.name"
                        "java.vm.specification.name"
                        "java.vm.specification.vendor"
                        "java.vm.specification.version"
                        "java.vm.vendor"
                        "java.vm.version" ]]
      (printf "%s=%s\n" prop-name (get p prop-name)))))


(defn bmp-codepoints
  []
  (concat (range 0 (int Character/MIN_SURROGATE))           ; 0x d800
          (range (inc (int Character/MAX_SURROGATE))        ; 0x dfff
                 Character/MIN_SUPPLEMENTARY_CODE_POINT)))  ; 0x10000


(defn supplementary-codepoints
  []
  (range Character/MIN_SUPPLEMENTARY_CODE_POINT
         (inc Character/MAX_CODE_POINT)))                   ; 0x10ffff


(defn all-codepoints-with-surrogates
  "Return a lazy sequence of all Unicode code points, both in the
   basic multilingual plane, and the supplementary characters.
   Surrogate codepoints are included in the sequence."
  []
  (concat (range 0 (inc Character/MAX_CODE_POINT))))


(defn all-codepoints
  "Return a lazy sequence of all Unicode code points, both in the
   basic multilingual plane, and the supplementary characters.  No
   surrogate codepoints will be included in the sequence."
  []
  (concat (bmp-codepoints) (supplementary-codepoints)))


(defn cp-to-utf16 [codepoint]
  (if (u/bmp-codepoint? codepoint)
    [ codepoint ]
    (let [v (- codepoint Character/MIN_SUPPLEMENTARY_CODE_POINT)  ; 0x10000
          vh (bit-shift-right v 10)
          vl (bit-and v 0x3FF)
          w1 (+ 0xD800 vh)
          w2 (+ 0xDC00 vl)]
      [ w1 w2 ])))


(defn hex-codeunit-str
  "Take string s and return a string consisting of s's 16-bit UTF-16
   code units, shown as hexadecimal numbers, separated by spaces.
   Useful for showing the precise contents of strings containing
   Unicode characters that do not show up in your system's default
   font."
  [s]
  (if s
    (str/join " " (map #(format "%X" (int %)) s))))


(defn hex-cp
  [cp]
  (if (u/bmp-codepoint? cp)
    (format "%04X" cp)
    (format "%06X" cp)))


(defn hex-codepoint-str
  "Take string s and return a string consisting of s's Unicode code points,
   shown as hexadecimal numbers, separated by spaces.  Useful for
   showing the precise contents of strings containing Unicode
   characters that do not show up in your system's default font."
  [s]
  (if s
    (str/join " " (map #(format "%X" %) (u/codepoints s)))))
