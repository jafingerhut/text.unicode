(ns com.fingerhutpress.text.unicode.test
  (:require [com.fingerhutpress.text.unicode :as u])
  (:use [clojure.test])
  (:import (java.util.regex PatternSyntaxException))
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.pprint :as p]))

(set! *warn-on-reflection* true)


(defn java-runtime-version []
  (let [[major minor patch]
        (map #(Long/parseLong %) (rest (re-find #"^(\d+)\.(\d+)\.(\d+)"
                                                (get (System/getProperties)
                                                     "java.runtime.version"))))]
    {:major major, :minor minor, :patch patch}))


;; Some interesting boundary values, as strings.
(def MIN_CODE_POINT_STR
     (u/chr Character/MIN_CODE_POINT))                ; U+0000
(def MAX_BMP_CODE_POINT_STR (u/chr 0xFFFF))
(def MIN_SUPPLEMENTARY_CODE_POINT_STR
     (u/chr Character/MIN_SUPPLEMENTARY_CODE_POINT))  ; U+10000
(def MAX_CODE_POINT_STR
     (u/chr Character/MAX_CODE_POINT))                ; U+10FFFF

;; Some arbitrarily chosen supplementary characters to use in testing.
(def COMBINING_GRAVE_ACCENT_STR (u/chr 0x0300))
(def MUSICAL_SYMBOL_G_CLEF_STR (u/chr 0x1D11E))
(def SMILING_FACE_WITH_OPEN_MOUTH_STR (u/chr 0x1F603))
(def BABY_ANGEL_STR (u/chr 0x1F47C))

;; Not valid UTF-16 strings, since they only contain a single
;; surrogate character.  Useful for building up other test cases of
;; invalid UTF-16 strings, and testing boundary cases.
(def MIN_LEADING_SURROGATE_STR (str Character/MIN_HIGH_SURROGATE))
(def MAX_LEADING_SURROGATE_STR (str Character/MAX_HIGH_SURROGATE))
(def MIN_TRAILING_SURROGATE_STR (str Character/MIN_LOW_SURROGATE))
(def MAX_TRAILING_SURROGATE_STR (str Character/MAX_LOW_SURROGATE))


;; Because the value of these symbols were calculated using u/chr, these
;; are effectively unit tests for the u/chr function.

(deftest test-chr
  (is (= "A" (u/chr 65)))
  (is (= '( "\n" "\r" "\t" ) (map u/chr [10 13 9])))
  (is (= "The quick brown fox jumped over the lazy dog"
         (apply str (map u/chr [84 104 101 32 113 117 105 99 107 32 98
                                114 111 119 110 32 102 111 120 32 106
                                117 109 112 101 100 32 111 118 101 114
                                32 116 104 101 32 108 97 122 121 32 100
                                111 103]))))
  (is (= "\ud834\udd1e" (u/chr 0x1d11e)))
  (is (= "\ud83d\ude03 smiling face"
         (apply str (map u/chr [0x1f603 32 115 109 105 108 105 110 103 32 102
                                97 99 101]))))
  (is (thrown? IllegalArgumentException (u/chr -1)))
  (is (thrown? IllegalArgumentException (u/chr (inc Character/MAX_CODE_POINT))))
  (is (thrown? IllegalArgumentException (u/chr (Integer/MAX_VALUE))))
  (is (thrown? IllegalArgumentException (u/chr (Integer/MIN_VALUE))))
  (is (= "\u0000" MIN_CODE_POINT_STR))
  (is (= "\uffff" MAX_BMP_CODE_POINT_STR))
  (is (= "\ud800\udc00" MIN_SUPPLEMENTARY_CODE_POINT_STR))
  (is (= "\udbff\udfff" MAX_CODE_POINT_STR))
  (is (= "\u0300" COMBINING_GRAVE_ACCENT_STR))
  (is (= "\ud834\udd1e" MUSICAL_SYMBOL_G_CLEF_STR))
  (is (= "\ud83d\ude03" SMILING_FACE_WITH_OPEN_MOUTH_STR))
  (is (= "\ud83d\udc7c" BABY_ANGEL_STR)))


(deftest test-ord-quick
  (are [i] (= i (u/ord (u/chr i)))
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


(deftest ^:slow test-ord-slow
  (doseq [i (all-codepoints)]
    (let [s (u/chr i)]
      (is (= i (u/ord s)))
      (is (= s (format "%c" (Integer. (int i)))))
      (is (= true (u/utf16? s))))))


(deftest test-bmp-codepoint?
  (is (= true (u/bmp-codepoint? 0)))
  (is (= true (u/bmp-codepoint? 0xffff)))
  (is (= false (u/bmp-codepoint? 0x10000)))
  (is (= false (u/bmp-codepoint? -1))))


(deftest ^:slow test-2-bmp-codepoint?
  (doseq [i (bmp-codepoints)]
    (is (= true (u/bmp-codepoint? i))))
  (doseq [i (supplementary-codepoints)]
    (is (= false (u/bmp-codepoint? i)))))


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
    (is (= nil (u/first-utf16-error s))))
  (is (= nil
         (u/first-utf16-error "\u0300 combining grave accent (not a surrogate)")))
  (is (= [0 :orphan-leading-surrogate]
           (u/first-utf16-error "\uD83D only leading surrogate")))
  (is (= [0 :orphan-trailing-surrogate]
           (u/first-utf16-error "\uDE03 only trailing surrogate")))
  (is (= [23 :orphan-leading-surrogate]
           (u/first-utf16-error "only leading surrogate \uD83D")))
  (is (= [16 :orphan-leading-surrogate]
           (u/first-utf16-error (str "two consecutive "
                                     MIN_LEADING_SURROGATE_STR
                                     MAX_LEADING_SURROGATE_STR
                                     " leading surrogates"))))
  (is (= [16 :orphan-trailing-surrogate]
           (u/first-utf16-error (str "two consecutive "
                                     MIN_TRAILING_SURROGATE_STR
                                     MAX_TRAILING_SURROGATE_STR
                                     " trailing surrogates"))))
  )


(deftest test-bad-surrogate-at-either-end?
  (let [f (fn [expected-answer s]
            (= expected-answer (u/bad-surrogate-at-either-end? s)))]
    (doseq [s valid-utf16-strings]
      (is (f false s)))
    (is (f false "\u0300 combining grave accent (not a surrogate)"))
    (is (f false "\uD83D only leading surrogate"))
    (is (f true "\uD83D"))
    (is (f true "\uDE03 only trailing surrogate"))
    (is (f true "\uDE03"))
    (is (f false "\uD83D\uDE03"))
    (is (f true "only leading surrogate \uD83D"))
    (is (f false (str "two consecutive "
                      MIN_LEADING_SURROGATE_STR
                      MAX_LEADING_SURROGATE_STR
                      " leading surrogates")))
    (is (f false (str "two consecutive "
                      MIN_TRAILING_SURROGATE_STR
                      MAX_TRAILING_SURROGATE_STR
                      " trailing surrogates")))
    ))


(deftest test-escape-supp
  (doseq [s valid-utf16-strings]
    (if (u/contains-supp? s)
      ;; Replace supplementary characters that might be there with
      ;; their escapes using a different slower method.
      (let [s2 (-> s
                   (str/replace (re-pattern MUSICAL_SYMBOL_G_CLEF_STR)
                                (format "<U+%06X>" (u/ord MUSICAL_SYMBOL_G_CLEF_STR)))
                   (str/replace (re-pattern SMILING_FACE_WITH_OPEN_MOUTH_STR)
                                (format "<U+%06X>" (u/ord SMILING_FACE_WITH_OPEN_MOUTH_STR)))
                   (str/replace (re-pattern BABY_ANGEL_STR)
                                (format "<U+%06X>" (u/ord BABY_ANGEL_STR)))
                   (str/replace (re-pattern MIN_SUPPLEMENTARY_CODE_POINT_STR)
                                (format "<U+%06X>" (u/ord MIN_SUPPLEMENTARY_CODE_POINT_STR)))
                   (str/replace (re-pattern MAX_CODE_POINT_STR)
                                (format "<U+%06X>" (u/ord MAX_CODE_POINT_STR)))
                   )]
        (is (= s2 (u/escape-supp s))))
      ;; If there are no supplementary characters, escape-supp should
      ;; return the original string.
      (is (= s (u/escape-supp s)))))
  
  (is (= "\u0300 combining grave accent (not a surrogate)"
         (u/escape-supp "\u0300 combining grave accent (not a surrogate)")))
  (is (= "<U+D83D> only leading surrogate"
         (u/escape-supp "\uD83D only leading surrogate")))
  (is (= "<U+DE03> only trailing surrogate"
         (u/escape-supp "\uDE03 only trailing surrogate")))
  (is (= "only leading surrogate <U+D83D>"
         (u/escape-supp "only leading surrogate \uD83D")))
  (is (= (str "two consecutive "
              (format "<U+%04X>" (int (.charAt ^String MIN_LEADING_SURROGATE_STR 0)))
              (format "<U+%04X>" (int (.charAt ^String MAX_LEADING_SURROGATE_STR 0)))
              " leading surrogates")
         (u/escape-supp (str "two consecutive "
                             MIN_LEADING_SURROGATE_STR
                             MAX_LEADING_SURROGATE_STR
                             " leading surrogates"))))
  (is (= (str "two consecutive "
              (format "<U+%04X>" (int (.charAt ^String MIN_TRAILING_SURROGATE_STR 0)))
              (format "<U+%04X>" (int (.charAt ^String MAX_TRAILING_SURROGATE_STR 0)))
              " trailing surrogates")
         (u/escape-supp (str "two consecutive "
                             MIN_TRAILING_SURROGATE_STR
                             MAX_TRAILING_SURROGATE_STR
                             " trailing surrogates"))))
  )


(deftest test-codepoints
  (is (= "61 300 1234 4567 1B1B 1D11E"
         (hex-codeunit-str
          (u/codepoints "a\u0300\u1234\u4567\u1b1b\ud834\udd1e"))))
  (is (= "0 300 FFFF 10000 10FFFF 1D11E"
         (hex-codeunit-str
          (u/codepoints "\u0000\u0300\uffff\ud800\udc00\udbff\udfff\ud834\udd1e"))))
  (is (= "1F603 20 73 6D 69 6C 69 6E 67 20 66 61 63 65"
         (hex-codeunit-str (u/codepoints "\uD83D\uDE03 smiling face")))))


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
  (map u/chr (u/codepoints s)))


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
    (let [s (apply str (repeat 3 (u/chr i)))]
      (is (= (repeat 3 (u/chr i)) (cp-strings-via-codepoints s)))
      (is (= (repeat 3 (u/chr i)) (cp-strings-via-regex s))))))


;; Clojure's clojure.string/reverse actually reverses valid UTF-16
;; strings to valid UTF-16 strings, even if there are surrogate pairs
;; in the string.  Test this, using codepoints.
(deftest test-reverse-and-codepoints
  (doseq [s valid-utf16-strings]
    (is (= (str/reverse s)
           (apply str (map u/chr (reverse (u/codepoints s))))))))


(defn contains-supp?-slower
  [^CharSequence s]
  (first (filter #(<= (int Character/MIN_SURROGATE)
                      % (int Character/MAX_SURROGATE))
                 (map int s))))


(deftest test-contains-supp?
  (doseq [s valid-utf16-strings]
    (is (= (u/contains-supp? s) (contains-supp?-slower s)))))


(deftest test-cp-count
  (doseq [s valid-utf16-strings]
    (is (= (u/cp-count s) (count (cp-strings-via-regex s))))))


(deftest ^:slow test-cp-count-slow
  (doseq [i (all-codepoints)]
    (let [s (u/chr i)]
      (is (= 1 (u/cp-count s)))
      (is (= (if (u/bmp-codepoint? i) 1 2) (count s))))))


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
    (let [n (u/cp-count s)]
      (doseq [start (maybe-partial-range 0 n)]
        (let [fast (u/cp-subs s start)
              medium (cp-subs-likely-slower-than-java s start)
              slow (cp-subs-via-codepoints s start)]
          (is (= fast medium slow))
          (is (= true (u/utf16? fast)))))
      (doseq [start (cons n (maybe-partial-range 0 n))
              end (cons n (maybe-partial-range start n))]
        (let [fast (u/cp-subs s start end)
              medium (cp-subs-likely-slower-than-java s start end)
              slow (cp-subs-via-codepoints s start end)]
          (is (= fast medium slow))
          (is (= true (u/utf16? fast)))))
      (is (thrown? StringIndexOutOfBoundsException (u/cp-subs s -1)))
      (is (thrown? StringIndexOutOfBoundsException (u/cp-subs s (inc n))))
      (is (thrown? StringIndexOutOfBoundsException (u/cp-subs s 0 (inc n)))))))


(defn ^String ccs-subs-slow?
  ([^CharSequence s start]
     (cond (neg? start)
           (throw (StringIndexOutOfBoundsException.))
           (zero? start)
           s
           :else
           (let [pat (re-pattern
                      (str "^(?:.\\pM*)"
                           (if (== start 1)
                             ""
                             (str "(?:\\PM\\pM*){" (dec start) "}"))))]
             ;;(printf "ccs-subs-slow pat='%s'\n" (str pat)) (flush)
             (if-let [remove (re-find pat s)]
               (subs s (count remove))
               (throw (StringIndexOutOfBoundsException.))))))
  ([^CharSequence s start end]
     (cond (or (neg? start) (< end start))
           (throw (StringIndexOutOfBoundsException.))
           (= start end)
           ""
           (zero? start)
           (let [pat (re-pattern
                      (str "^(?:.\\pM*)"
                           (if (== end 1)
                             ""
                             (str "(?:\\PM\\pM*){" (dec end) "}"))))]
             ;;(printf "ccs-subs-slow pat='%s'\n" (str pat)) (flush)
             (if-let [s1 (re-find pat s)]
               s1
               (throw (StringIndexOutOfBoundsException.))))
           :else    ;; 1 <= start < end
           (let [pat (re-pattern
                      (str "^(?:.\\pM*)"
                           (if (== start 1)
                             ""
                             (str "(?:\\PM\\pM*){" (dec start) "}"))
                           "((?:\\PM\\pM*){" (- end start) "})"))]
             ;;(printf "ccs-subs-slow pat='%s'\n" (str pat)) (flush)
             (if-let [[_ s1] (re-find pat s)]
               s1
               (throw (StringIndexOutOfBoundsException.)))))))


(deftest test-ccs-subs
  (let [ccs-cp-lists
        [
         [ 0xe007f ]  ; CANCEL TAG + 0 combining chars
         [ 0x0020 0x6df ]  ; SPACE + 1 combining char
         [ 0xa07b 0x1d1ad ]  ; YI SYLLABLE NBIE + 1 combining char
         [ 0x0041 0x0300 0x0309 ] ; A + 2 combining chars
         [ 0x10000 0x651 0xfb1e 0xe01ef ] ; LINEAR B SYLLABLE + 3 combining
         ]
        ccss (vec (map (fn [cp-list] (apply str (map u/chr cp-list)))
                       ccs-cp-lists))

        ccs-vec1 [ (ccss 0) (ccss 1) (ccss 2) (ccss 3) (ccss 4) ]
        ccs-vec2 [ (ccss 1) (ccss 2) (ccss 3) (ccss 4) ]
        ccs-vec3 [ (u/cp-subs (ccss 1) 1)  ; first 1 char is combining char
                   (ccss 2) (ccss 3) (ccss 4) ]
        ccs-vec3 [ (u/cp-subs (ccss 4) 1)  ; first 3 chars are combining chars
                   (ccss 2) (ccss 3) (ccss 0) ]
        ]
    (doseq [ccs-vec [ ccs-vec1 ccs-vec2 ccs-vec3 ]]
      (let [num-ccs (count ccs-vec)
            s (apply str ccs-vec)]
        ;;(printf "\n")
        (doseq [start (range 0 (inc num-ccs))
                end (range start (inc num-ccs))]
;;          (printf "start=%d end=%d (u/cp-count (u/ccs-subs s start end))=%d\n"
;;                  start end (u/cp-count (u/ccs-subs s start end))) (flush)
          (is (= (u/ccs-subs s start end)
                 (ccs-subs-slow? s start end)
                 (apply str (subvec ccs-vec start end)))))
        (doseq [start (range 0 (inc num-ccs))]
          ;;(printf "start=%d end=none\n" start) (flush)
          (is (= (u/ccs-subs s start)
                 (ccs-subs-slow? s start)
                 (apply str (subvec ccs-vec start))))
          ;;(printf "start=%d end=%d should be exception\n" start (inc num-ccs)) (flush)
          (is (thrown? StringIndexOutOfBoundsException
                       (u/ccs-subs s start (inc num-ccs))))
          ;;(printf "start=%d end=%d should be exception\n" start (inc num-ccs)) (flush)
          (is (thrown? StringIndexOutOfBoundsException
                       (ccs-subs-slow? s start (inc num-ccs))))
          )
        (is (thrown? StringIndexOutOfBoundsException
                     (u/ccs-subs s (inc num-ccs))))
        (is (thrown? StringIndexOutOfBoundsException
                     (ccs-subs-slow? s (inc num-ccs))))
        (is (thrown? StringIndexOutOfBoundsException
                     (u/ccs-subs s (inc num-ccs) (+ num-ccs 2))))
        (is (thrown? StringIndexOutOfBoundsException
                     (ccs-subs-slow? s (inc num-ccs) (+ num-ccs 2))))))))


(defn cp-escape-slow [s cmap]
  (let [strmap (reduce (fn [m [cp x]]
                         (assoc m (u/chr cp) x))
                       {} cmap)]
    (apply str (map #(get strmap % %) (cp-strings-via-regex s)))))


(deftest test-cp-escape
  (let [f (fn [s cmap] (= (u/cp-escape s cmap) (cp-escape-slow s cmap)))]
    (let [cmap {(u/ord MUSICAL_SYMBOL_G_CLEF_STR) "<MUSICAL SYMBOL G CLEF>",
                (u/ord COMBINING_GRAVE_ACCENT_STR) "<COMBINING GRAVE ACCENT>"
                (u/ord BABY_ANGEL_STR) "<BABY ANGEL>"}
          s1 (str "a" COMBINING_GRAVE_ACCENT_STR "\u1234\u4567\u1b1b"
                 MUSICAL_SYMBOL_G_CLEF_STR)]
      (is (= (str "a" "<COMBINING GRAVE ACCENT>" "\u1234\u4567\u1b1b"
                  "<MUSICAL SYMBOL G CLEF>")
             (u/cp-escape s1 cmap)
             (cp-escape-slow s1 cmap)))
      (doseq [s valid-utf16-strings]
        (is (= (u/cp-escape s cmap) (cp-escape-slow s cmap)))))))


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
    (is (= "61 300 1234 4567 1B1B D834 DD1E" (hex-codeunit-str s)))
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
    (is (= "1B1B" (f #"\u1b1b")
           (f (re-pattern "\\u1b1b"))
           (f (re-pattern  "\u1b1b"))))

    (is (= "61 300" (f #"\u0061\u0300")
           (f (re-pattern "\\u0061\\u0300"))
           (f (re-pattern  "\u0061\u0300"))))
    (is (= "1234 4567 1B1B" (f #"\u1234\u4567\u1b1b")
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
    (is (= "DD1E" (f #"\udd1e")
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

      (is (= "D834 DD1E" (f #"\ud834\udd1e")
                         (f (re-pattern "\\ud834\\udd1e"))
                         (f (re-pattern "\\x{1D11E}"))))

      ;; 1.6 behavior I've seen, and perhaps earlier
      (do
        (is (=  nil   (f #"\ud834\udd1e")
                      (f (re-pattern "\\ud834\\udd1e"))))
        (is (thrown? PatternSyntaxException (f (re-pattern "\\x{1D11E}"))))))
    ;; test case E
    (is (= "D834 DD1E" (f (re-pattern "\ud834\udd1e"))))

    ;; It can find it by searching for the regex . at the end of the
    ;; string, which is good.
    (is (= "D834 DD1E" (f #".$")))))


;; Looks like the String constructor from an int array of codepoints
;; allows even surrogates and 0xFFFE and 0xFFFF to be codepoints.
(deftest ^:slow test-java-lang-string-constructor-invalid-codepoints
  (let [a (int-array 1)]
    (doseq [c (all-codepoints-with-surrogates)]
      (aset a 0 (int c))
      (let [s (String. a 0 1)]
        (is (= (if (u/bmp-codepoint? c) 1 2) (count s))))))
  (is (thrown? IllegalArgumentException
               (String. (int-array 1 (dec Character/MIN_CODE_POINT)) 0 1)))
  (is (thrown? IllegalArgumentException
               (String. (int-array 1 (inc Character/MAX_CODE_POINT)) 0 1))))


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
  (let [fname "char-categories.txt"]
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (print-interesting-jvm-version-properties)
        (printf "\n")
        (let [similar-char-groups
              (->> (all-codepoints)
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


(defn legal-pattern
  [s]
  (try
    (re-pattern s)
    (catch PatternSyntaxException e nil)))


;; WARNING: Running this test will fetch a file from the unicode.org
;; web site. It is about 126 Kbytes in size, so not huge.

(deftest ^:test-unicode-property-names test-unicode-property-names
  (let [in-fname
;        "http://unicode.org/Public/4.0-Update/Scripts-4.0.0.txt"
;        "http://unicode.org/Public/4.1.0/ucd/Scripts.txt"
;        "http://unicode.org/Public/5.0.0/ucd/Scripts.txt"
;        "http://unicode.org/Public/5.1.0/ucd/Scripts.txt"
;        "http://unicode.org/Public/5.2.0/ucd/Scripts.txt"
;        "http://unicode.org/Public/6.0.0/ucd/Scripts.txt"
;        "http://unicode.org/Public/6.1.0/ucd/Scripts-6.1.0d13.txt"

;        "/Users/andy/clj/www.unicode.org/Public/zipped/4.1.0/UCD/Scripts.txt"
;        "/Users/andy/clj/www.unicode.org/Public/zipped/6.0.0/UCD/Scripts.txt"

        ;; At least with Hotspot Java 1.6.0_29 provided by Apple with
        ;; Mac OS X 10.6.8, the \p{InFoo} syntax in a regular
        ;; expression appears to correspond almost exactly with the
        ;; Block specifications from Blocks.txt in Unicode 4.1.0.  The
        ;; only difference is that there are a few Block names in that
        ;; version of the Blocks.txt file that are not supported by
        ;; this JVM.
        
        ;; The Scripts.txt files above specifies script names, many of
        ;; which have the same name as Block names, but specify very
        ;; different sets of characters.
        "http://unicode.org/Public/4.1.0/ucd/Blocks.txt"
        ;; local copy on my machine:
        ;; "/Users/andy/clj/www.unicode.org/Public/zipped/4.1.0/UCD/Blocks.txt"

        out-fname "unicode-property-names-test-out.txt"
        num-all-cps (count (all-codepoints))]
    (with-open [rdr (io/reader in-fname)
                wr (io/writer out-fname :encoding "UTF-8")]
      (binding [*out* wr]
        (print-interesting-jvm-version-properties)
        (printf "\n")
        (let [script-map
              (->> (line-seq rdr)
                   ;; Assign line numbers
                   (map-indexed (fn [idx line]
                                  {:line-num (inc idx) :line line}))
                   ;; Remove comments beginning at first #, and ignore blank
                   ;; or comment-only lines.
                   (remove #(str/blank? (str/replace-first (:line %) #"#.*" "")))
                   ;; Replace lines with map of info extracted from the line
                   (map (fn [{:keys [line] :as m}]
                          (if-let [[_ first-cp last-cp script-name]
                                   (re-find #"(?x)
                                      ^ \s*
                                      ([0-9a-fA-F]+)         # first code point
                                      (?:[.;\ ]+([0-9a-fA-F]+))? # last code
                                                             # point of a range
                                      \s* ; \s*              # field separator
                                      ([^\#]+)               # script name
                                      \s*
                                      (?:\#.*)?              # optional comment
                                      $"
                                            line)]
                            ;; add more fields to this line's map
                            (merge m {:first-cp (Integer/parseInt first-cp 16),
                                      :last-cp (Integer/parseInt
                                                (or last-cp first-cp) 16),
                                      :script-name script-name})
                            ;; else just keep the original fields without adding any
                            (do
                              (printf "Unrecognized format on line %d: %s\n" (:line-num m) line)
                              m))))
                   (group-by :script-name))]
          (doseq [script-name-prefix-in-re-pattern ["" "Is" "Block=" "In"]]
            (printf "\n\nTry prefix \"%s\" before script name in regex pattern:\n"
                    script-name-prefix-in-re-pattern)
            (doseq [script-name (sort (keys script-map))]
              (when script-name
                (let [re-string (str "^\\p{" script-name-prefix-in-re-pattern
                                     (str/replace script-name #" " "") "}$")]
                  (if-let [pat (legal-pattern re-string)]
                    (do
                      (printf "Regex %s is legal\n" re-string)
                      (let [should-match-cps
                            (set (mapcat (fn [{:keys [first-cp last-cp]}]
                                           (range first-cp (inc last-cp)))
                                         (script-map script-name)))
                            do-match-cps (set (filter #(re-find pat (u/chr %))
                                                      (all-codepoints)))
                            good-match-cps (set/intersection should-match-cps
                                                             do-match-cps)
                            should-match-but-dont-cps
                            (set/difference should-match-cps good-match-cps)

                            shouldnt-match-but-do-cps
                            (set/difference do-match-cps good-match-cps)
                            ]
                        (printf "    %d should match.  %d do, %d do not.  %d that should not match, do\n"
                                (count should-match-cps)
                                (count good-match-cps)
                                (count should-match-but-dont-cps)
                                (count shouldnt-match-but-do-cps))
                        (when (not= 0 (count should-match-but-dont-cps))
                          (printf "    Should match, but do not: %s\n"
                                  (str/join " "
                                            (map hex-cp
                                                 (sort should-match-but-dont-cps)))))
                        (when (not= 0 (count shouldnt-match-but-do-cps))
                          (printf "    Should not match, but do: %s\n"
                                  (str/join " "
                                            (map hex-cp
                                                 (sort shouldnt-match-but-do-cps)))))
;                        (printf "    Code points that should match, but don't:\n")
;                        (printf "      %s\n"
;                                (str/join "\n      " (map #(format "%06X" %)
;                                                          non-matching-cps)))
                        )
;                    (doseq [{:keys [first-cp last-cp]}
;                            (script-map script-name)]
;                      (doseq [cp (range first-cp (inc last-cp))]
;                        (let [s (u/chr cp)]
;                          (when (not (re-find pat s))
;                            (printf "    U+%06X should match, but doesn't\n" cp)))))
                      )
                    ;; else
                    (printf "Regex %s is NOT legal\n" re-string)))))))))))


(deftest ^:write-normalized-forms-to-file
  write-normalized-forms-to-file
  (let [fname "normalized-form-data.txt"]
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (print-interesting-jvm-version-properties)
        (printf "\n")
        (let [normalized-forms
              (->> (all-codepoints)
                   (map (fn [i] {:cp i :str (u/chr i)}))
                   (map (fn [m] (assoc m
                                  :nfc (u/NFC (:str m))
                                  :nfd (u/NFD (:str m))
                                  :nfkc (u/NFKC (:str m))
                                  :nfkd (u/NFKD (:str m))))))]
          (printf "hex-codepoint
;string S, containing that code point and nothing else
;max # of codepoints in either NFC or NFD of S
;hex-codepoints of NFC(S), if different from S, otherwise empty
;NFC(S)
;hex-codepoints of NFD(S), if different from S, otherwise empty
;NFD(S)
;max # of codepoints in either NFKC or NFKD of S
;hex-codepoints of NFKC(S), if different from S, otherwise empty
;NFKC(S)
;hex-codepoints of NFKD(S), if different from S, otherwise empty
;NFKD(S)

")
          (doseq [m normalized-forms]
            (when (not (= (:str m) (:nfc m) (:nfd m) (:nfkc m) (:nfkd m)))
              (printf "%s"
               (str (format "%06X" (:cp m))
                    (format ";%s" (:str m))
                    (format ";%d" (max (u/cp-count (:nfc m)) (u/cp-count (:nfd m))))
                    (format ";%s" (if (= (:str m) (:nfc m))
                                    "" (hex-codepoint-str (:nfc m))))
                    (format ";%s" (if (= (:str m) (:nfc m))
                                    "" (:nfc m)))
                    (format ";%s" (if (= (:str m) (:nfd m))
                                    "" (hex-codepoint-str (:nfd m))))
                    (format ";%s" (if (= (:str m) (:nfd m))
                                    "" (:nfd m)))

                    (format ";%d" (max (u/cp-count (:nfkc m)) (u/cp-count (:nfkd m))))
                    (format ";%s" (if (= (:str m) (:nfkc m))
                                    "" (hex-codepoint-str (:nfkc m))))
                    (format ";%s" (if (= (:str m) (:nfkc m))
                                    "" (:nfkc m)))
                    (format ";%s" (if (= (:str m) (:nfkd m))
                                    "" (hex-codepoint-str (:nfkd m))))
                    (format ";%s" (if (= (:str m) (:nfkd m))
                                    "" (:nfkd m)))
                    "\n")

               ))))))))
