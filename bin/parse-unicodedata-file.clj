#! /usr/bin/env clj

(ns parse-unicodedata-file.core
  (require
;   [com.fingerhutpress.clj-perls :as cp]
   [clojure.set :as set]
   [clojure.java.io :as io]))


(when (not (and (= 2 (count *command-line-args*))
                (re-matches #"\d+" (nth *command-line-args* 1))))
;  (cp/die (format "usage: %s <UnicodeData.txt file loc> <min-gap-size-to-print>\n" *file*))
  (binding [*out* *err*]
    (printf "usage: %s <UnicodeData.txt file loc> <min-gap-size-to-print>\n" *file*)
    (flush)
    (System/exit 1)))


(defn take-first-range
  "Look through the beginning of the given collection, via seq,
   for the longest prefix such that the first item is x, the second
   item is (f x), the third is (f (f x)), etc.

   Return a list ((first last n) rest-of-coll), where first is the
   first item of this prefix, last is the last item of this prefix,
   and n is the number of items in this prefix.  The prefix will be
   length 1, with first=last, if the second item is not equal to (f
   first).  rest-of-coll is a seq of the rest of the collection,
   starting with the first item that was not part of the prefix.

   Returns nil for an empty collection.

   Examples:
   user=> (take-first-range inc [0 1 2 3 4])
   ((0 4 5))
   user=> (take-first-range inc [0 1 2 3 5])
   ((0 3 4) 5)
   user=> (take-first-range inc [0 2 3 5])
   ((0 0 1) 2 3 5)
   user=> (take-first-range inc [0])
   ((0 0 1))
   user=> (take-first-range inc [])
   nil"
  [f coll]
  (if-let [s (seq coll)]
    (let [fst (first s)]
      (loop [s (next s)
             lst fst
             exp (f fst)
             n 1]
        (if-let [s (seq s)]
          ;; then
          (if (= (first s) exp)
            (recur (next s) exp (f exp) (inc n))
            (cons (list fst lst n) s))
          ;; else
          (cons (list fst lst n) s))))
    nil))


(defn partition-into-ranges
  "TBD: Document"
  [f coll]
  (lazy-seq
   (when-let [[first-range & rst] (take-first-range f coll)]
     (cons first-range (partition-into-ranges f rst)))))


(defn find-int-ranges
  "TBD: Document"
  [int-coll]
  (partition-into-ranges inc int-coll))


;(defn find-int-ranges
;  "TBD: Document"
;  [int-coll]
;  (lazy-seq
;   (if-let [[[fst lst n] & rst] (take-first-range inc int-coll)]
;     (cons (list fst lst) (find-int-ranges rst)))))


(let [fname (nth *command-line-args* 0)
      min-gap-size (Integer/parseInt (nth *command-line-args* 1))]
  (with-open [rdr (io/reader fname)]
    (let [
;          cp-list (->> (line-seq rdr)
;                       (map #(Integer/parseInt (re-find #"^[0-9a-fA-F]+" %) 16)))
;          cp-set (set cp-list)
          cp-set (->> (line-seq rdr)
                      (map #(Integer/parseInt (re-find #"^[0-9a-fA-F]+" %) 16))
                      (set))
          _ (do (printf "(count cp-set)=%d\n" (count cp-set)) (flush))
          min-cp (apply min cp-set)
          max-cp (apply max cp-set)
          _ (do (printf "min-cp=%d  max-cp=%d\n" min-cp max-cp) (flush))
          missing-cps (set/difference (set (range min-cp (inc max-cp)))
                                      cp-set)]
      (doseq [[fst lst n] (find-int-ranges (sort missing-cps))]
        (let [gap-size (inc (- lst fst))]
          (when (>= gap-size min-gap-size)
            (printf "%06X ... %06X  %5d\n" fst lst (inc (- lst fst))))))
;      (doseq [cp (sort missing-cps)]
;        (printf "%06X\n" cp))
      )))
