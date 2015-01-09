(ns com.fingerhutpress.text.unicode.test-whitespace
  (:use [clojure.test])
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :as p]
            [com.fingerhutpress.text.unicode :as u]
            [com.fingerhutpress.text.unicode.test-helpers :as h]))

(set! *warn-on-reflection* true)


;; Names for Unicode code points that were included in the output of
;; this test with Oracle JDK 1.7.0_45 on Mac OS X 10.9.5.  I doubt the
;; set of characters in the output will change very often for this.

(def cp->name
  {
   0x0009 "tab (horizontal)"
   0x000A "line feed"
   0x000B "vertical tab"
   0x000C "form feed"
   0x000D "carriage return"
   0x001C "information separator four"
   0x001D "information separator three"
   0x001E "information separator two"
   0x001F "information separator one"
   0x0020 "space"
   0x00A0 "non-breaking space"
   0x1680 "Ogham space mark"
   0x180E "Mongolian vowel separator"
   0x2000 "en quad"
   0x2001 "em quad"
   0x2002 "en space"
   0x2003 "em space"
   0x2004 "three-per-em space"
   0x2005 "four-per-em space"
   0x2006 "six-per-em space"
   0x2007 "figure space"
   0x2008 "punctuation space"
   0x2009 "thin space"
   0x200A "hair space"
   0x2028 "line separator"
   0x2029 "paragraph separator"
   0x202F "narrow no-break space"
   0x205F "medium mathematical space"
   0x3000 "ideographic space"
   })


(def max-name-len (apply max (map (comp count val) cp->name)))


(deftest ^:whitespace write-whitespace-info-to-file
  (let [fname "whitespace.txt"]
    (println (format "Writing file '%s' ..." fname))
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (h/print-interesting-jvm-version-properties)
        (println "Meaning of words below:")
        (printf "\n")
        (println "  isSpaceChar - method Character/isSpaceChar(int) returns true for the codepoint")
        (println "  isWhitespace - method Character/Whitespace(int) returns true for the codepoint")
        (println "  re-s - character matches regex #\"\\s\"")
        (println "  re-p-Space - character matches regex #\"\\p{Space}\"")
        (println "  re-p-javaWhitespace - character matches regex #\"\\p{javaWhitespace}\"")
        (println "  re-p-Z - character matches regex #\"\\pZ\"")
        (printf "\n")
        (println (format "# of code points tested: %d (decimal)"
                         (count (h/all-codepoints))))
        (println (format "Highest code point tested: %04X (hex)"
                         (last (h/all-codepoints))))
        (println (format "max name length: %d chars"
                         max-name-len))
        (printf "\n")
        (doseq [cp (h/all-codepoints)
                :let [s (u/chr cp)
                      char-isSpaceChar (Character/isSpaceChar (int cp))
                      char-isWhitespace (Character/isWhitespace (int cp))
                      re-s (re-find #"^\s$" s)
                      re-p-Space (re-find #"^\p{Space}$" s)
                      re-p-javaWhitespace (re-find #"^\p{javaWhitespace}$" s)
                      re-p-Z (re-find #"^\pZ$" s)
                      ;; Character/isSpace is a deprecated method,
                      ;; probably because it was created before
                      ;; extension to > 2^16 Unicode characters was
                      ;; added.
                      ;; (Character/isSpace cp)
                      ]
                :when (or re-s
                          re-p-Space
                          re-p-javaWhitespace
                          re-p-Z
                          char-isSpaceChar
                          char-isWhitespace)]
          
          (printf "%04X" cp)
          (printf (if re-s
                    " re-s"
                    "     "))
          (printf (if re-p-Space
                    " re-p-Space"
                    "           "))
          (printf (if char-isSpaceChar
                    " isSpaceChar"
                    "            "))
          (printf (if re-p-Z
                    " re-p-Z"
                    "       "))
          (printf (if char-isWhitespace
                    " isWhitespace"
                    "             "))
          (printf (if re-p-javaWhitespace
                    " re-p-javaWhitespace"
                    "                    "))
          (printf " %-30s" (if (contains? cp->name cp)
                             (cp->name cp)
                             "unknown-name"))
          (printf "\n")
          )))))
