(ns com.fingerhutpress.text.unicode.test-property-names
  (:import (java.util.regex PatternSyntaxException))
  (:use [clojure.test])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [com.fingerhutpress.text.unicode :as u]
            [com.fingerhutpress.text.unicode.test-helpers :as h]))

(set! *warn-on-reflection* true)


(defn legal-pattern
  [s]
  (try
    (re-pattern s)
    (catch PatternSyntaxException e nil)))


;; WARNING: Running this test will fetch a file from the unicode.org
;; web site. It is about 126 Kbytes in size, so not huge.

(deftest ^:property-names test-unicode-property-names
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

        out-fname "property-names.txt"
        num-all-cps (count (h/all-codepoints))]
    (with-open [rdr (io/reader in-fname)
                wr (io/writer out-fname :encoding "UTF-8")]
      (println (format "Writing file '%s' ..." out-fname))
      (binding [*out* wr]
        (h/print-interesting-jvm-version-properties)
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
                                                      (h/all-codepoints)))
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
                                            (map h/hex-cp
                                                 (sort should-match-but-dont-cps)))))
                        (when (not= 0 (count shouldnt-match-but-do-cps))
                          (printf "    Should not match, but do: %s\n"
                                  (str/join " "
                                            (map h/hex-cp
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
