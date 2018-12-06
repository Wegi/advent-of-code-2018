(ns advent.polymers
  (:require [clojure.string :as str]))

(def polymer (slurp "./polymer.edn"))
polymer
(defn- reagents?
  ([[first second]]
   (reagents? first second))
  ([first second]
   (when (and (not= first
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
  ())

(react polymer)
;;11894
