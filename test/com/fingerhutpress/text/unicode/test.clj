(ns com.fingerhutpress.text.unicode.test
  (:use [com.fingerhutpress.text.unicode])
  (:use [clojure.test])
  (:import (java.util.regex PatternSyntaxException))
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :as p]))


(defn java-runtime-version []
  (let [[major minor patch]
        (map #(Long/parseLong %) (rest (re-find #"^(\d+)\.(\d+)\.(\d+)"
                                                (get (System/getProperties)
                                                     "java.runtime.version"))))]
    {:major major, :minor minor, :patch patch}))


;; TBD: See if I can use com.fingerhutpress.clj-perls where chr and
;; ord are defined, instead of redefining it here.  DRY.

(defn ^String chr
  "Return a string containing only the one specified Unicode code point,
   although it may contain 1 or 2 UTF-16 code units."
  [codepoint]
  (String. (Character/toChars codepoint)))


(defn ord
  "Return the Unicode code point of the first character in string s.
   If the first character is a UTF-16 surrogate pair, the code point
   returned is that of the pair, not of the leading surrogate.  Return
   0 if the string is empty.

   The behavior is undefined if the string is not valid UTF-16."
  [s]
  (if (= s "")   ; special case for Perl compatability
    0
    (.codePointAt s 0)))


;; Some interesting boundary values, as strings.
(def MIN_CODE_POINT_STR
     (chr Character/MIN_CODE_POINT))                ; U+0000
(def MAX_BMP_CODE_POINT_STR (chr 0xFFFF))
(def MIN_SUPPLEMENTARY_CODE_POINT_STR
     (chr Character/MIN_SUPPLEMENTARY_CODE_POINT))  ; U+10000
(def MAX_CODE_POINT_STR
     (chr Character/MAX_CODE_POINT))                ; U+10FFFF

;; Some arbitrarily chosen supplementary characters to use in testing.
(def COMBINING_GRAVE_ACCENT_STR (chr 0x0300))
(def MUSICAL_SYMBOL_G_CLEF_STR (chr 0x1D11E))
(def SMILING_FACE_WITH_OPEN_MOUTH_STR (chr 0x1F603))
(def BABY_ANGEL_STR (chr 0x1F47C))

;; Not valid UTF-16 strings, since they only contain a single
;; surrogate character.  Useful for building up other test cases of
;; invalid UTF-16 strings, and testing boundary cases.
(def MIN_LEADING_SURROGATE_STR (str Character/MIN_HIGH_SURROGATE))
(def MAX_LEADING_SURROGATE_STR (str Character/MAX_HIGH_SURROGATE))
(def MIN_TRAILING_SURROGATE_STR (str Character/MIN_LOW_SURROGATE))
(def MAX_TRAILING_SURROGATE_STR (str Character/MAX_LOW_SURROGATE))


;; Because the value of these symbols were calculated using chr, these
;; are effectively unit tests for the chr function.

(deftest test-chr
  (= "\u0000" MIN_CODE_POINT_STR)
  (= "\uffff" MAX_BMP_CODE_POINT_STR)
  (= "\ud800\udc00" MIN_SUPPLEMENTARY_CODE_POINT_STR)
  (= "\udbff\udfff" MAX_CODE_POINT_STR)
  (= "\u0300" COMBINING_GRAVE_ACCENT_STR)
  (= "\ud834\udd1e" MUSICAL_SYMBOL_G_CLEF_STR)
  (= "\ud83d\ude03" SMILING_FACE_WITH_OPEN_MOUTH_STR)
  (= "\ud83d\udc7c" BABY_ANGEL_STR))


(deftest test-ord-quick
  (are [i] (= i (ord (chr i)))
       Character/MIN_CODE_POINT
       (dec (int Character/MIN_HIGH_SURROGATE))
       (inc (int Character/MAX_LOW_SURROGATE))
       (dec Character/MIN_SUPPLEMENTARY_CODE_POINT)
       Character/MIN_SUPPLEMENTARY_CODE_POINT
       Character/MAX_CODE_POINT))


;; TBD: Create a namespace specifically for utilities that are useful
;; for testing other Unicode-related functions, but might not be
;; desired to have in a namespace for everyday use.

;; Candidates for inclusion there:

;; bmp-codepoints
;; supplementary-codepoints
;; all-codepoints
;; all-codepoints-with-surrogates
;; cp-to-utf16
;; cp-strings-via-regex
;; cp-strings-via-codepoints
;; hex-codeunit-str


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
  (if (bmp-codepoint? codepoint)
    [ codepoint ]
    (let [v (- codepoint Character/MIN_SUPPLEMENTARY_CODE_POINT)  ; 0x10000
          vh (bit-shift-right v 10)
          vl (bit-and v 0x3FF)
          w1 (+ 0xD800 vh)
          w2 (+ 0xDC00 vl)]
      [ w1 w2 ])))


(defn hex-codeunit-str
  "Take string s and return a string consisting of s's 16-bit UTF-16
   code units, in shown as hexadecimal numbers, separated by spaces.
   Useful for showing the precise contents of strings containing
   Unicode characters that do not show up in your system's default
   font."
  [s]
  (if s
    (str/join " " (map #(format "%x" (int %)) s))))


(deftest ^:slow test-ord-slow
  (doseq [i (all-codepoints)]
    (is (= i (ord (chr i))))
    (is true (utf16? (chr i)))))


(deftest test-bmp-codepoint?
  (is (= true (bmp-codepoint? 0)))
  (is (= true (bmp-codepoint? 0xffff)))
  (is (= false (bmp-codepoint? 0x10000)))
  (is (= false (bmp-codepoint? -1))))


(deftest ^:slow test-2-bmp-codepoint?
  (doseq [i (bmp-codepoints)]
    (is (= true (bmp-codepoint? i))))
  (doseq [i (supplementary-codepoints)]
    (is (= false (bmp-codepoint? i)))))


(def valid-utf16-strings
     [""
      "no surrogate code units"
      "put some\tcontrol\ncharacter  \r in to see if they shake\banything up."
      (str "a" COMBINING_GRAVE_ACCENT_STR "\u1234\u4567\u1b1b"
           MUSICAL_SYMBOL_G_CLEF_STR)
      (str MIN_CODE_POINT_STR COMBINING_GRAVE_ACCENT_STR
           MAX_BMP_CODE_POINT_STR MIN_SUPPLEMENTARY_CODE_POINT_STR
           MAX_CODE_POINT_STR BABY_ANGEL_STR)
      (str SMILING_FACE_WITH_OPEN_MOUTH_STR " smiling face")
      ;; Test a string long enough that it is likely to cause any
      ;; implementations that are not lazy, and not tail recursive, to
      ;; exceed the maximum stack depth.
      (apply str (repeat 10000 (str "ABCD123" MUSICAL_SYMBOL_G_CLEF_STR)))
      ])


(deftest test-first-utf16-error
  (doseq [s valid-utf16-strings]
    (is (= nil (first-utf16-error s))))
  (is (= nil
         (first-utf16-error "\u0300 combining grave accent (not a surrogate)")))
  (is (= [0 :orphan-leading-surrogate]
           (first-utf16-error "\uD83D only leading surrogate")))
  (is (= [0 :orphan-trailing-surrogate]
           (first-utf16-error "\uDE03 only trailing surrogate")))
  (is (= [23 :orphan-leading-surrogate]
           (first-utf16-error "only leading surrogate \uD83D")))
  (is (= [16 :orphan-leading-surrogate]
           (first-utf16-error (str "two consecutive "
                                   MIN_LEADING_SURROGATE_STR
                                   MAX_LEADING_SURROGATE_STR
                                   " leading surrogates"))))
  (is (= [16 :orphan-trailing-surrogate]
           (first-utf16-error (str "two consecutive "
                                   MIN_TRAILING_SURROGATE_STR
                                   MAX_TRAILING_SURROGATE_STR
                                   " trailing surrogates"))))
  )


(deftest test-codepoints
  (is (= "61 300 1234 4567 1b1b 1d11e"
         (hex-codeunit-str
          (codepoints "a\u0300\u1234\u4567\u1b1b\ud834\udd1e"))))
  (is (= "0 300 ffff 10000 10ffff 1d11e"
         (hex-codeunit-str
          (codepoints "\u0000\u0300\uffff\ud800\udc00\udbff\udfff\ud834\udd1e"))))
  (is (= "1f603 20 73 6d 69 6c 69 6e 67 20 66 61 63 65"
         (hex-codeunit-str (codepoints "\uD83D\uDE03 smiling face")))))


(defn cp-strings-via-regex
  "If the input string s has (utf16? s) true, returns a lazy sequence
   of strings, where each string contains a single Unicode code point.
   Thus each one contains either (a) a single Java char that is not a
   surrogate, or (b) two Java chars where the first is a leading
   surrogate and the second is a trailing surrogate.  This includes
   substrings that are line terminators such as \"\\n\" and \"\\r\".

   The behavior is undefined if the string is not valid UTF-16."
  [^String s]
  (re-seq #"(?s)." s))


(defn cp-strings-via-codepoints
  [^String s]
  (map chr (codepoints s)))


;; I wrapped these two calls in seq because these two are not equal
;; without the seq call:

;; (cp-strings-via-codepoints "") = '()  (class clojure.lang.LazySeq)
;; (cp-strings-via-regex "") = nil

;; TBD: Is there a Clojure 'standard' for whether a Clojure function
;; returning a lazy sequence should return '() or nil for an empty
;; sequence?

(deftest test-2-codepoints
  (doseq [s valid-utf16-strings]
    (is (= (seq (cp-strings-via-codepoints s))
           (seq (cp-strings-via-regex s))))))


(deftest ^:slow test-3-codepoints-slow
  (doseq [i (all-codepoints)]
    (let [s (apply str (repeat 3 (chr i)))]
      (is (= (repeat 3 (chr i)) (cp-strings-via-codepoints s)))
      (is (= (repeat 3 (chr i)) (cp-strings-via-regex s))))))


;; Clojure's clojure.string/reverse actually reverses valid UTF-16
;; strings to valid UTF-16 strings, even if there are surrogate pairs
;; in the string.  Test this, using codepoints.
(deftest test-reverse-and-codepoints
  (doseq [s valid-utf16-strings]
    (is (= (str/reverse s)
           (apply str (map chr (reverse (codepoints s))))))))


(defn contains-supp?-slower
  [^CharSequence s]
  (first (filter #(<= (int Character/MIN_SURROGATE)
                      % (int Character/MAX_SURROGATE))
                 (map int s))))


(deftest test-contains-supp?
  (doseq [s valid-utf16-strings]
    (is (= (contains-supp? s) (contains-supp?-slower s)))))


(deftest test-cp-count
  (doseq [s valid-utf16-strings]
    (is (= (cp-count s) (count (cp-strings-via-regex s))))))


(deftest ^:slow test-cp-count-slow
  (doseq [i (all-codepoints)]
    (let [s (chr i)]
      (is (= 1 (cp-count s)))
      (is (= (if (bmp-codepoint? i) 1 2) (count s))))))


(defn cp-subs-via-codepoints
  "Slow but straightforward implementation of subs with code point
   indices instead of Java char indices, intended only to compare its
   results against the results of cp-subs, which should be faster."
  ([s start]
     (apply str (subvec (vec (cp-strings-via-regex s)) start)))
  ([s start end]
     (apply str (subvec (vec (cp-strings-via-regex s)) start end))))


(defn- find-cp-idx
  [^String s len start-idx start-cp-count target-cp-count]
  (loop [i start-idx
         cp-count start-cp-count]
    (if (< cp-count target-cp-count)
      (if (< i len)
        (let [c (.charAt s i)]
          (if (Character/isHighSurrogate c)
            (if (< (inc i) len)
              ;; then assume next character is
              ;; corresponding low surrogate
              (recur (+ i 2) (inc cp-count))
              ;; else high/leading surrogate is last code
              ;; unit in the string.  It isn't valid
              ;; UTF-16, and the given code point index is
              ;; after the end of the string.
              (throw (StringIndexOutOfBoundsException.)))
            ;; else c is in BMP, so code point is only 1
            ;; UTF-16 code unit.
            (recur (inc i) (inc cp-count))))
        ;; start code point index is after end of string
        (throw (StringIndexOutOfBoundsException.)))
      i)))

  
(defn ^String cp-subs-likely-slower-than-java
  "Returns the substring of s beginning at start inclusive, and ending
   at end (defaults to the length of the string), exclusive.

   Unlike subs, the start and end indices are in units of code points,
   not UTF-16 code units.  If s is a valid UTF-16 string, start and
   end are in the range [0, (count-codepoints s)], and start <= end,
   the return value is guaranteed to be a valid UTF-16 string.

   Under the assumption below, subs-codepoints takes time linear in
   the portion of the input string that it must scan to find the
   appropriate UTF-16 code unit index (or indices).  When called with
   only a start index, this is linear in the value start.  When called
   with both a start and end index, it is linear in the value end.

   Assumption: Java's java.lang.String substring method takes constant
   time, regardless of the length of the input string and the values
   of the start and end indices, because it can construct a new string
   that shares character data with the input string (both are
   immutable, so this is safe)."
  ;; TBD: See if I can add more details to the exceptions thrown, like
  ;; the value of the index, such as subs does.
  ([^String s start]
     (if (neg? start)
       (throw (StringIndexOutOfBoundsException.))
       (let [len (.length s)
             start-idx (find-cp-idx s len 0 0 start)]
         (subs s start-idx))))
  ([^String s start end]
     (if (<= 0 start end)
       (let [len (.length s)
             start-idx (find-cp-idx s len 0 0 start)
             ;; continue looking for end-idx from where we left off
             end-idx (find-cp-idx s len start-idx start end)]
         (subs s start-idx end-idx))
       (throw (StringIndexOutOfBoundsException.)))))


;; Always include the extreme values, but no more than about 10 values
;; total, to avoid excessively long test times.
(defn maybe-partial-range [minim maxim]
  (cons maxim (range minim maxim (if (<= (- maxim minim) 10)
                                   1
                                   (quot (- (+ maxim 9) minim) 10)))))


(deftest test-cp-subs
  (doseq [s valid-utf16-strings]
    (let [n (cp-count s)]
      (doseq [start (maybe-partial-range 0 n)]
        (let [fast (cp-subs s start)
              medium (cp-subs-likely-slower-than-java s start)
              slow (cp-subs-via-codepoints s start)]
          (is (= fast medium slow))
          (is (= true (utf16? fast)))))
      (doseq [start (cons n (maybe-partial-range 0 n))
              end (cons n (maybe-partial-range start n))]
        (let [fast (cp-subs s start end)
              medium (cp-subs-likely-slower-than-java s start end)
              slow (cp-subs-via-codepoints s start end)]
          (is (= fast medium slow))
          (is (= true (utf16? fast)))))
      (is (thrown? StringIndexOutOfBoundsException (cp-subs s -1)))
      (is (thrown? StringIndexOutOfBoundsException (cp-subs s (inc n))))
      (is (thrown? StringIndexOutOfBoundsException (cp-subs s 0 (inc n)))))))


;; The test below "passes" with this software:

;; Mac OS X 10.6.8
;; Java 1.6.0_29
;; Clojure 1.3.0

;; I doubt changing the OS or Clojure version will change the results,
;; since most of the behavior here is due to the implementation of
;; regex matching in Java.  Changing the Java version might get
;; different results.

(deftest test-java-unicode-regex-matching-strangeness
  (let [s (str "a" COMBINING_GRAVE_ACCENT_STR "\u1234\u4567\u1b1b"
               MUSICAL_SYMBOL_G_CLEF_STR)
        jre-version (java-runtime-version)
        f (fn [pat] (hex-codeunit-str (re-find pat s)))]

    ;; These test results all make sense to me.  It can find any
    ;; single BMP character, or a sequence of them, using the \uxxxx
    ;; syntax to specify the codepoint, or by putting the exact
    ;; Unicode character into the pattern with re-pattern.
    (is (= "61 300 1234 4567 1b1b d834 dd1e" (hex-codeunit-str s)))
    (is (=   "61" (f #"\u0061")
           (f (re-pattern "\\u0061"))   ; equivalent to prev line
           (f (re-pattern  "\u0061")))) ; pattern has only 1 character, not 6
    (is (=  "300" (f #"\u0300")
           (f (re-pattern "\\u0300"))
           (f (re-pattern  "\u0300"))))
    (is (= "1234" (f #"\u1234")
           (f (re-pattern "\\u1234"))
           (f (re-pattern  "\u1234"))))
    (is (= "4567" (f #"\u4567")
           (f (re-pattern "\\u4567"))
           (f (re-pattern  "\u4567"))))
    (is (= "1b1b" (f #"\u1b1b")
           (f (re-pattern "\\u1b1b"))
           (f (re-pattern  "\u1b1b"))))

    (is (= "61 300" (f #"\u0061\u0300")
           (f (re-pattern "\\u0061\\u0300"))
           (f (re-pattern  "\u0061\u0300"))))
    (is (= "1234 4567 1b1b" (f #"\u1234\u4567\u1b1b")
           (f (re-pattern "\\u1234\\u4567\\u1b1b"))
           (f (re-pattern  "\u1234\u4567\u1b1b"))))

    ;; The following several are strange.  It can't find a leading
    ;; surrogate by itself, but it can find the trailing surrogate by
    ;; itself.

    ;; I suppose I don't care too much about the results here, since
    ;; it is probably not a good idea to use regex matching to look
    ;; for individual UTF-16 surrogates, anyway.

    ;; test case A
    (is (=  nil   (f #"\ud834")
                  (f (re-pattern "\\ud834"))
                  (f (re-pattern  "\ud834"))))
    ;; test case B
    (is (= "dd1e" (f #"\udd1e")
           (f (re-pattern "\\udd1e"))))
    ;; test case C
    (is (=  nil   (f (re-pattern "\udd1e"))))

    ;; TBD: From testing on only a few JVMs, it appears that perhaps
    ;; JDK 1.7 and later can find a whole Unicode character by
    ;; specifying its two surrogates exactly, using \uxxxx in the
    ;; pattern, but JDK 1.6 and earlier cannot.

    ;; It seems likely this was considered a bug in JDK 1.6 that was
    ;; fixed in 1.7.
    
    ;; Workaround: It can find the whole Unicode character by putting
    ;; the actual Unicode character into the pattern itself, not the
    ;; \uxxxx escape sequence.  This requires using re-pattern in
    ;; Clojure.

    ;; test case D
    (if (or (> (:major jre-version) 1)
            (and (== (:major jre-version) 1) (> (:minor jre-version) 6)))

      ;; 1.7 and later behavior I've seen.

      ;; Note that support for \x{HHHHHH} syntax seems to have been
      ;; added for regular expressions in Java 7, but to take
      ;; advantage of it you must use re-pattern, and escape the
      ;; backslash so that the backslash character gets passed through
      ;; to the Java method for compiling regex patterns.

      ;; I'm not aware of any plans to include this syntax in regular
      ;; Java strings, or in the Clojure reader for strings and regex
      ;; patterns, although it would be nice to have.  I think if it
      ;; were added to the Clojure reader, using \x{HHHHHH} would be a
      ;; little bit strange, in that if you were to use the code point
      ;; for a special regex character like ( ) * + [ ], it would have
      ;; its meaning as that special character, and not be escaped.
      ;; That would best be mentioned in the documentation.

      (is (= "d834 dd1e" (f #"\ud834\udd1e")
                         (f (re-pattern "\\ud834\\udd1e"))
                         (f (re-pattern "\\x{1D11E}"))))

      ;; 1.6 behavior I've seen, and perhaps earlier
      (do
        (is (=  nil   (f #"\ud834\udd1e")
                      (f (re-pattern "\\ud834\\udd1e"))))
        (is (thrown? PatternSyntaxException (f (re-pattern "\\x{1D11E}"))))))
    ;; test case E
    (is (= "d834 dd1e" (f (re-pattern "\ud834\udd1e"))))

    ;; It can find it by searching for the regex . at the end of the
    ;; string, which is good.
    (is (= "d834 dd1e" (f #".$")))))


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


;; Note: write-char-types-to-file isn't really so much a unit test as
;; a mechanism of dumping info that the JVM stores about each
;; character out to a file.  It is human readable, but perhaps better
;; used for post-processing and/or comparing against Unicode data
;; files, or against the output of a similar program in another
;; language like Perl to see how their character info compares to each
;; other.

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


(defn print-cp-info-line [cp-info]
;  (printf "print-cp-info-line: cp-info=")
;  (p/pprint cp-info)
;  (flush)
  (printf "U+%06x %2d %-5s %s\n" (cp-info :cp)
          (cp-info :java-enum-int-val)
          (get-in num-to-cat-info [(cp-info :java-enum-int-val)
                                   :short-java-char-type-name])
          (cp-info :matching-cat-list)))


(deftest ^:write-char-types-to-file write-char-types-to-file
  (let [fname "char-type-data.txt"]
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (print-interesting-jvm-version-properties)
        (printf "\n")
        (let [similar-char-groups
              (->> (all-codepoints)
                   ;; Create a map for each codepoint with some info
                   (map (fn [i] {:cp i
                                 :java-enum-int-val (Character/getType i)
                                 :str (chr i)}))
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