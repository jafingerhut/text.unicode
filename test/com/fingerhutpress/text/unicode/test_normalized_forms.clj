(ns com.fingerhutpress.text.unicode.test-normalized-forms
  (:use [clojure.test])
  (:require [clojure.java.io :as io]
            [com.fingerhutpress.text.unicode :as u]
            [com.fingerhutpress.text.unicode.test-helpers :as h]))

(set! *warn-on-reflection* true)


(deftest ^:normalized-forms
  write-normalized-forms-to-file
  (let [fname "normalized-forms.txt"]
    (println (format "Writing file '%s' ..." fname))
    (with-open [f (io/writer fname :encoding "UTF-8")]
      (binding [*out* f]
        (h/print-interesting-jvm-version-properties)
        (printf "\n")
        (let [normalized-forms
              (->> (h/all-codepoints)
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
                                    "" (h/hex-codepoint-str (:nfc m))))
                    (format ";%s" (if (= (:str m) (:nfc m))
                                    "" (:nfc m)))
                    (format ";%s" (if (= (:str m) (:nfd m))
                                    "" (h/hex-codepoint-str (:nfd m))))
                    (format ";%s" (if (= (:str m) (:nfd m))
                                    "" (:nfd m)))

                    (format ";%d" (max (u/cp-count (:nfkc m)) (u/cp-count (:nfkd m))))
                    (format ";%s" (if (= (:str m) (:nfkc m))
                                    "" (h/hex-codepoint-str (:nfkc m))))
                    (format ";%s" (if (= (:str m) (:nfkc m))
                                    "" (:nfkc m)))
                    (format ";%s" (if (= (:str m) (:nfkd m))
                                    "" (h/hex-codepoint-str (:nfkd m))))
                    (format ";%s" (if (= (:str m) (:nfkd m))
                                    "" (:nfkd m)))
                    "\n")

               ))))))))
