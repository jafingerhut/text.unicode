(ns com.fingerhutpress.text.unicode
  (:require [clojure.string :as str]))


(defn bmp-codepoint? [c]
  (and (<= 0 c) (< c Character/MIN_SUPPLEMENTARY_CODE_POINT)))


;; codepoints is a slight adaptation from a function of the same name
;; that used to be in clojure.contrib.string.  It has been modified to
;; be lazy, whereas the original linked below could easily exceed the
;; maximum stack depth for long strings.

;; Source:
;; https://github.com/richhickey/clojure-contrib/blob/master/src/main/clojure/clojure/contrib/string.clj

;; docodepoints is from the same place.

(defn codepoints
  "Returns a lazy sequence of integer Unicode code points in s.
   Handles Unicode supplementary characters (above U+FFFF) correctly."
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
   supplementary characters (above U+FFFF) correctly."
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



;; TBD: Define for ^CharSequence?

(defn cp-count
  "Returns the length of s in Unicode code points.  This can be
   smaller than (count s), if s contains UTF-16 surrogate pairs.

   The behavior is undefined if the string is not valid UTF-16."
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
   the value of end if end is specified, otherwise start.

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
