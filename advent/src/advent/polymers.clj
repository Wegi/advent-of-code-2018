(ns advent.polymers
  (:require [clojure.string :as str]))

(def polymer (slurp "./polymer.edn"))

(defn- reagents?
  ([[first second]]
   (reagents? first second))
  ([first second]
   (when (and (not (nil? first))
              (not= first
                    second)
              (= (str/lower-case first)
                 (str/lower-case second)))
     true)))

(defn react
  [original-polymer]
  (loop [pos 0
         polymer (vec original-polymer)]
    (if (<= (count polymer) (inc pos))
      (count polymer) ;; return case
      (if (reagents? (get polymer pos) (get polymer (inc pos)))
        (recur (max 0 (dec pos)) (vec (concat (take pos polymer) (drop (+ pos 2) polymer))))
        (recur (inc pos) polymer)))))

(defn fast-react
  ;; Think of a faster version
  [original-polymer]
  (count
   (reduce
    (fn [scanned-string testchar]
      (let [polymer (vec scanned-string)]
        ;; last was slower
        (if (reagents? (peek polymer) testchar)
          ;; drop-last was even slower
          (pop polymer)
          (conj polymer testchar))))
    []
    original-polymer)))
;; Version ohne vectoren braucht etwa eine bis zwei minuten

(defn filter-char
  "Expects the lowercase char"
  [polymer char]
  (let [upper-char (first (str/upper-case char))]
    (filter #(not (#{char upper-char} %)) polymer)))

(defn filtered-combinations
  "Filter for every char and five back all possibilities."
  [polymer]
  (map filter-char (repeat polymer) (distinct (str/lower-case polymer))))

(defn find-smallest-polymer
  [polymer]
  (apply min
   (map fast-react (filtered-combinations polymer))))

#_(find-smallest-polymer polymer)
;;5310

#_(fast-react polymer)
#_(react polymer)
;;11894
