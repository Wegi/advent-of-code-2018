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

(def state-as-map
  (zipmap (range (count initial-state)) (map str initial-state)))

(defn- extend-state
  [state]
  (let [minn (apply min-key key state)]
    (if (= (second minn) "#")
      (if (= (get state (inc (first minn))) "#")
        (merge state {(dec (first minn)) "."})
        (merge state {(dec (first minn)) "." (- (first minn) 2) "."}))
      state)))

(defn tick
  [rules state]
  (let [initial (extend-state state)
        minn (first (apply min-key key initial))
        sequence (range minn (+ (count initial) minn 2))
        patterns (map
                  #(str (get initial (+ % -2) ".")
                        (get initial (+ % -1) "." )
                        (get initial % ".")
                        (get initial (+ % 1) ".")
                        (get initial (+ % 2) "."))
                  sequence)]
    (into {} (map #(vector %1 (get rules %2))
                  sequence
                  patterns))))

(defn tick-times
  [num rules state]
  (take (inc num) (iterate (partial tick rules) state)))

(defn print-plants
  [state]
  (println (apply str (vals (into (sorted-map) state)))))

(defn part-1
  [num rules state]
  (let [plants (last (tick-times num rules state))]
    (doall (map print-plants (tick-times num rules state)))
    (reduce #(if (= "#" (second %2))
               (+ %1 (first %2))
               %1)
            0 plants)))

#_(time (part-1 20 rules state-as-map))
;;3258
