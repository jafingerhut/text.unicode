(ns com.fingerhutpress.text.unicode.test
  (:use [com.fingerhutpress.text.unicode])
  (:use [clojure.test])
  (:require [clojure.string :as str]))


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
;; unicode-codepoint-to-utf16-codeunits
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


(defn unicode-codepoint-to-utf16-codeunits [codepoint]
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


(deftest test-cp-count
  (doseq [s valid-utf16-strings]
    (is (= (cp-count s) (count (cp-strings-via-regex s))))))


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

    ;; TBD: It cannot find the whole Unicode character by specifying
    ;; its two surrogates exactly, using \uxxxx in the pattern.  This
    ;; seems like a bug to me.  It can find the whole Unicode
    ;; character by putting the actual Unicode character into the
    ;; pattern itself, not the \uxxxx escape sequence.  This requires
    ;; using re-pattern in Clojure.  This could be considered a
    ;; workaround, I suppose.

    ;; test case D
    (is (=  nil   (f #"\ud834\udd1e")
                  (f (re-pattern "\\ud834\\udd1e"))))
    ;; test case E
    (is (= "d834 dd1e" (f (re-pattern "\ud834\udd1e"))))

    ;; It can find it by searching for the regex . at the end of the
    ;; string, which is good.
    (is (= "d834 dd1e" (f #".$")))))
