(ns advent.day-12
  (:require [clojure.string :as str]))

(def input
  (str/split (slurp "in_day12") #"\n"))

(def initial-state
  (get (str/split (first input) #" ") 2))

(def rules
  (into {} (map
            #(str/split % #" => ")
            (drop 2 input))))
