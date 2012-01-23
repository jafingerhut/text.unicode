(ns com.fingerhutpress.text.unicode
  (:import (java.text Normalizer))
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)


(defmacro bmp-codepoint? [c]
  `(let [cp# ~c]
     (and (<= 0 cp#) (< cp# Character/MIN_SUPPLEMENTARY_CODE_POINT))))


;; codepoints is a slight adaptation from a function of the same name
;; that used to be in clojure.contrib.string.  It has been modified to
;; be lazy, whereas the original linked below could easily exceed the
;; maximum stack depth for long strings.

;; Source:
;; https://github.com/richhickey/clojure-contrib/blob/master/src/main/clojure/clojure/contrib/string.clj

;; docodepoints is from the same place.

(defn codepoints
  "Returns a lazy sequence of integer Unicode code points in s.
   Handles Unicode supplementary characters (U+10000 and above)
   correctly."
  [^String s]
  (let [len (.length s)
        f (fn thisfn [^String s i]
            (lazy-seq
             (when (< i len)
               (let [c (.charAt s i)]
                 (if (Character/isHighSurrogate c)
                   (cons (.codePointAt s i) (thisfn s (+ 2 i)))
                   (cons (int c) (thisfn s (inc i))))))))]
    (f s 0)))


(defmacro docodepoints
  "bindings => [name string]

   Repeatedly executes body, with name bound to the integer code point
   of each Unicode character in the string.  Handles Unicode
   supplementary characters (U+10000 and above) correctly.

   The behavior is undefined if the string is not valid UTF-16, as
   determined by the function utf16?"
  [bindings & body]
  (assert (vector bindings))
  (assert (= 2 (count bindings)))
  (let [character (first bindings)
        string (second bindings)]
    `(let [#^String s# ~string
           len# (.length s#)]
       (loop [i# 0]
         (when (< i# len#)
           (let [~character (.charAt s# i#)]
             (if (Character/isHighSurrogate ~character)
               (let [~character (.codePointAt s# i#)]
                 ~@body
                 (recur (+ 2 i#)))
               (let [~character (int ~character)]
                 ~@body
                 (recur (inc i#))))))))))


;; TBD: What about U+FFFE and U+FFFF characters?  Should strings
;; containing them be considered invalid UTF-16?
(defn first-utf16-error
  "Returns nil if s is a valid UTF-16 string, i.e. every leading (or
   high) surrogate, in the range U+D800 to U+DBFF inclusive, is
   immediately followed by a low (or trailing) surrogate, in the range
   U+DC00 to U+DFFF inclusive, and trailing surrogates appear nowhere
   else but immediately after a leading surrogate.

   If it is not a valid UTF-16 string, then the return value is a
   vector [i problem] where i is first index in the string where there
   is a violation of the rules, and problem is
   either :orphan-leading-surrogate or :orphan-trailing-surrogate.

   Examples:

   user=> (first-utf16-error \"no surrogate code units\")
   nil
   user=> (first-utf16-error \"\\uD83D\\uDE03 smiling face\")
   nil
   user=> (first-utf16-error \"\\uD83D smiling face\")
   [0 :orphan-leading-surrogate]
   user=> (first-utf16-error \"\\uDE03 smiling face\")
   [0 :orphan-trailing-surrogate]
   user=> (first-utf16-error \"smiling face: \\uD83D\")
   [14 :orphan-leading-surrogate]
   user=> (first-utf16-error \"\\u0300 combining grave accent (not a surrogate)\")
   nil"
  [^CharSequence s]
  (let [len (.length s)]
    (loop [i 0]
      (when (< i len)
        (let [c (.charAt s i)]
          (cond (Character/isHighSurrogate c)
                (let [i+1 (inc i)]
                  (if (and (< i+1 len)
                           (Character/isLowSurrogate (.charAt s i+1)))
                    (recur (+ i 2))
                    [i :orphan-leading-surrogate]))
                (Character/isLowSurrogate c)
                [i :orphan-trailing-surrogate]
                :else
                (recur (inc i))))))))


(defn utf16?
  "Returns true if the string or CharSequence s is a valid UTF-16
   sequence, otherwise false.

   See also first-utf16-error if you want a return value that gives
   details on why a string is not valid UTF-16, and documentation that
   includes examples of valid and invalid UTF-16 strings."
  [^CharSequence s]
  (nil? (first-utf16-error s)))


(defn ^String escape-supp
  "Return a new string, replacing Unicode supplementary characters (or
   code points), which require 2 Java chars to represent, with a
   string of the form <U+XXXXXX>, where XXXXXX is the hexadecimal code
   point value.  Do the same (except only use 4 hex digits) for
   unpaired surrogate characters, which should never appear in valid
   Unicode strings encoded as UTF-16."
  [^CharSequence s]
  (let [s (.toString s)
        len (count s)
        buffer (StringBuilder. len)]
    (loop [i 0]
      (if (< i len)
        (let [c (.charAt s i)]
          (cond (Character/isHighSurrogate c)
                (let [i+1 (inc i)]
                  (if (and (< i+1 len)
                           (Character/isLowSurrogate (.charAt s i+1)))
                    (do
                      (.append buffer (format "<U+%06X>" (.codePointAt s i)))
                      (recur (+ i 2)))
                    (do
                      (.append buffer (format "<U+%04X>" (int c)))
                      (recur (inc i)))))
                (Character/isLowSurrogate c)
                (do
                  (.append buffer (format "<U+%04X>" (int c)))
                  (recur (inc i)))
                :else
                (do
                  (.append buffer c)
                  (recur (inc i)))))
        (.toString buffer)))))


(defn contains-supp?
  "Returns logical true (see below) if the string or CharSequence s
   contains supplementary characters, outside the Basic Multilingual
   Plane.  Returns nil if the string only contains characters in the
   BMP.

   For Java/Clojure strings, which are encoded in UTF-16, a string
   contains supplementary characters only if the string contains at
   least one surrogate code unit in the range U+D800 through U+DFFF.
   If the string s contains such a UTF-16 surrogate code unit, the
   UTF-16 integer code unit of the first one in the string is
   returned."
  [^CharSequence s]
  (let [n (.length s)
        min-surr (int Character/MIN_SURROGATE)
        max-surr (int Character/MAX_SURROGATE)]
    (loop [i 0]
      (if (< i n)
        (let [c (.charAt s i)]
          (if (<= min-surr (int c) max-surr)
            (int c)
            (recur (inc i))))))))


;; TBD: Define for ^CharSequence?

(defn cp-count
  "Returns the length of s in Unicode code points.  This can be
   smaller than (count s), if s contains UTF-16 surrogate pairs.

   The behavior is undefined if the string is not valid UTF-16, as
   determined by the function utf16?"
  [^String s]
  (.codePointCount s 0 (count s)))

  
(defn ^String cp-subs-helper
  ([^String s start]
     (if (neg? start)
       (throw (StringIndexOutOfBoundsException.))
       (let [start-idx (.offsetByCodePoints s 0 start)]
         (subs s start-idx))))
  ([^String s start end]
     (if (<= 0 start end)
       (let [start-idx (.offsetByCodePoints s 0 start)
             ;; continue looking for end-idx from where we left off
             end-idx (.offsetByCodePoints s start-idx (- end start))]
         (subs s start-idx end-idx))
       (throw (StringIndexOutOfBoundsException.)))))

  
(defn ^String cp-subs
  "Returns the substring of s beginning at start inclusive, and ending
   at end (defaults to the length of the string), exclusive.

   Unlike subs, the start and end indices are in units of Unicode code
   points, not UTF-16 code units (i.e., Java chars).  If s is a valid
   UTF-16 string and start and end are in the proper range, the return
   value is guaranteed to be a valid UTF-16 string.

   Under the assumption below, cp-subs takes time linear in the
   portion of the input string that it must scan to find the
   appropriate UTF-16 code unit index (or indices).  This is linear in
   the value of end if end is specified, otherwise it is linear in the
   value of start.

   Assumption: Java's substring method in java.lang.String takes
   constant time, regardless of the length of the input string and the
   values of the start and end indices, because it can construct a new
   string that shares character data with the input string (both are
   immutable, so this is safe)."
  ;; TBD: See if I can add more details to the exceptions thrown, like
  ;; the value of the index, such as subs does.
  ([^String s start]
     (try
       (cp-subs-helper s start)
       (catch IndexOutOfBoundsException e
           (throw (StringIndexOutOfBoundsException.)))))
  ([^String s start end]
     (try
       (cp-subs-helper s start end)
       (catch IndexOutOfBoundsException e
           (throw (StringIndexOutOfBoundsException.))))))


(defmacro combining-cp? [cp]
  `(let [t# (Character/getType (int ~cp))]
     (or (== t# (int Character/NON_SPACING_MARK))
         (== t# (int Character/COMBINING_SPACING_MARK))
         (== t# (int Character/ENCLOSING_MARK)))))


(defmacro cp-at
  "Return integer codepoint of character beginning at index idx of s,
   if it lies completely within the string s.  Return :eos if idx is
   exactly at end of string, :past-eos if idx is larger than len,
   or :incomplete-supplementary-char if there is a high surrogate at
   the end of the string, with nothing after it.

   len is simply (.length s).  It is included as an argument since
   where it is used the length has already been extracted, and it is
   expected that it is faster to use the already-extracted length
   rather than invoking the method repeatedly."
  [^String s len idx]
  `(cond (== ~idx ~len) :eos
         (> ~idx ~len) :past-eos
         (Character/isHighSurrogate (.charAt ~s ~idx))
         (if (< (inc ~idx) ~len)
           (.codePointAt ~s ~idx)
           :incomplete-supplementary-char)
         :else (.codePointAt ~s ~idx)))


(defmacro cp-num-chars
  "Return the number of Java chars, which is the number of UTF-16 code
   units, required to encode the code point cp."
  [cp]
  `(if (bmp-codepoint? ~cp) 1 2))


(defn- find-ccs-idx
  [^String s len start-idx start-ccs-count target-ccs-count]
  (if (== start-ccs-count target-ccs-count)
    ;; Already done.  Return immediately.
    start-idx
    
    ;; Special case: If first character is combining character,
    ;; pretend that we already saw a combining character earlier by
    ;; incrementing ccs-count.  We have to check for the case that
    ;; there is no first character while we are doing this.  Implement
    ;; this by skipping over the first character and using (inc
    ;; start-ccs-count), whether the first character is combining or
    ;; not.
    (let [first-cp (cp-at s len start-idx)]
      (if (not (number? first-cp))
        (throw (StringIndexOutOfBoundsException.))
        (loop [i (+ start-idx (cp-num-chars first-cp))
               ccs-count (inc start-ccs-count)]
          (let [cp (cp-at s len i)]
            (cond
             (= cp :eos)
             (if (== ccs-count target-ccs-count)
               i
               (throw (StringIndexOutOfBoundsException.)))
             
             (number? cp)
             (cond
              (combining-cp? cp) (recur (+ i (cp-num-chars cp)) ccs-count)
              (== ccs-count target-ccs-count) i
              :else (recur (+ i (cp-num-chars cp)) (inc ccs-count)))
             
             :else (throw (StringIndexOutOfBoundsException.)))))))))


;; TBD: Add a ccs-count function that calculates the number of CCSs in
;; a string.  If we use the same simple rule for defining a CCS as
;; used in ccs-subs, this is as simple as counting all of the
;; characters that match \PM in the string, plus 1 if there is a
;; leading substring that matches \pM+.

;; Creating a version of ccs-count for a more Unicode-standard version
;; of a CCS is likely trickier.

(defn ^String ccs-subs
  "Returns the substring of s beginning at start inclusive, and ending
   at end (defaults to the end of the string), exclusive.

   Unlike subs, the start and end indices are in units of Unicode
   combining character sequences (CCSs), not UTF-16 code units (i.e.,
   Java chars).

   For the purposes of this function, a CCS is any character except a
   combining mark character, followed by 0 or more combining mark
   characters.  That is, it matches the regular expression:

      \\PM\\pM*

   This is not precisely the definition of a CCS in the Unicode 6.0.0
   standard, but may be close enough for some purposes.

   If s begins with one or more combining mark characters, all of them
   up to, but not including, the first non-combining mark character,
   will count as the first CCS, with index 0.

   If s is a valid UTF-16 string, and start and end are in the proper
   range, the return value is guaranteed to be a valid UTF-16 string."
  ([^CharSequence s start]
     (if (neg? start)
       (throw (StringIndexOutOfBoundsException.))
       (let [len (.length s)
             start-idx (find-ccs-idx s len 0 0 start)]
         (subs s start-idx))))
  ([^CharSequence s start end]
     (if (<= 0 start end)
       (let [len (.length s)
             start-idx (find-ccs-idx s len 0 0 start)
             ;; continue looking for end-idx from where we left off
             end-idx (find-ccs-idx s len start-idx start end)]
         (subs s start-idx end-idx))
       (throw (StringIndexOutOfBoundsException.)))))


(defn ^String cp-escape
  "Return a new string, using cmap to escape each Unicode code point
   ch from s as follows:
   
   If (cmap ch) is nil, append code point ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead.

   The keys of cmap should be integer code points, not Java characters
   or strings.

   The behavior is undefined if s is not a valid UTF-16 string, as
   determined by function utf16?

   Note that while clojure.string/escape is similar, it escapes UTF-16
   code units, or Java chars.  If you wish to escape a Unicode
   supplementary character, which requires 2 Java chars to represent,
   clojure.string/escape can escape those two Java chars
   independently, but not as a unit.  cp-escape can escape them as a
   unit."
  [^CharSequence s cmap]
  (let [buffer (StringBuilder. (count s))]
    (docodepoints [c s]
      (if-let [replacement (cmap c)]
        (.append buffer replacement)
        (.appendCodePoint buffer c)))
    (.toString buffer)))


(defn NFC
  "Return a string that is in Unicode Normalization Form C (NFC),
   by doing canonical decomposition, followed by canonical
   composition, on the input string.

   Every character that can be represented as either a single combined
   Unicode code point (e.g. a Latin A with an acute accent), or as a
   base character followed by one or more combining characters (e.g. a
   Latin A character, followed by a combining character for an acute
   accent), is turned into its decomposed form (e.g. the second form),
   where the combining characters are sorted into a standard-specified
   order, and then transformed into its composed form (e.g. like the
   first example form).

   For any two Unicode strings s1 and s2 that are canonically
   equivalent, (= (NFC s1) (NFC s2)) will be true, even if (= s1 s2)
   is false.

   See also: http://unicode.org/reports/tr15/
             http://en.wikipedia.org/wiki/Unicode_equivalence"
  [^CharSequence s]
  (Normalizer/normalize s java.text.Normalizer$Form/NFC))


(defn NFD
  "Return a string that is in Unicode Normalization Form D (NFD),
   by doing canonical decomposition on the input string.

   Every character that can be represented as either a single combined
   Unicode code point (e.g. a Latin A with an acute accent), or as a
   base character followed by one or more combining characters (e.g. a
   Latin A character, followed by a combining character for an acute
   accent), is turned into its decomposed form (e.g. the second form),
   where the combining characters are sorted into a standard-specified
   order.

   For any two Unicode strings s1 and s2 that are canonically
   equivalent, (= (NFD s1) (NFD s2)) will be true, even if (= s1 s2)
   is false.

   See also: http://unicode.org/reports/tr15/
             http://en.wikipedia.org/wiki/Unicode_equivalence"
  [^CharSequence s]
  (Normalizer/normalize s java.text.Normalizer$Form/NFD))


(defn NFKC
  "Return a string that is in Unicode Normalization Form KC (NFKC),
   by doing compatibility decomposition, followed by compatibility
   composition, on the input string.

   For any two Unicode strings s1 and s2 that are compatibility
   equivalent, (= (NFKC s1) (NFKC s2)) will be true, even if (= s1 s2)
   is false.

   See also: http://unicode.org/reports/tr15/
             http://en.wikipedia.org/wiki/Unicode_equivalence"
  [^CharSequence s]
  (Normalizer/normalize s java.text.Normalizer$Form/NFKC))


(defn NFKD
  "Return a string that is in Unicode Normalization Form KD (NFKD),
   by doing compatibility decomposition, followed by compatibility
   composition, on the input string.

   For any two Unicode strings s1 and s2 that are compatibility
   equivalent, (= (NFKD s1) (NFKD s2)) will be true, even if (= s1 s2)
   is false.

   See also: http://unicode.org/reports/tr15/
             http://en.wikipedia.org/wiki/Unicode_equivalence"
  [^CharSequence s]
  (Normalizer/normalize s java.text.Normalizer$Form/NFKD))
