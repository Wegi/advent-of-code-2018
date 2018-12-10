(ns advent.day-10
  (:require [clojure.string :as str]))

(def testdata
  (as-> (slurp "day10") v
    (str/split v #"\n")
    (map #(rest (re-find #"^position=<(.+), (.*)> velocity=<(.+), (.+)>$" %)) v)
    (map (fn [[px py vx vy]] {:pos-x (read-string px)
                             :pos-y (read-string py)
                             :vel-x (read-string vx)
                             :vel-y (read-string vy)}) v)))

(defn bounds
  [maps]
  (let [min-x (:pos-x (apply min-key :pos-x maps))
        max-x (:pos-x (apply max-key :pos-x maps))
        min-y (:pos-y (apply min-key :pos-y maps))
        max-y (:pos-y (apply max-key :pos-y maps))]
      [[min-x max-x] [min-y max-y]]))

(defn tick
  "Executes ticks for the given map."
  ([points]
   (tick points 1))
  ([points times]
   (map (fn [p]
          {:pos-x (+ (:pos-x p) (* times (:vel-x p)))
           :pos-y (+ (:pos-y p) (* times (:vel-y p)))
           :vel-x (:vel-x p)
           :vel-y (:vel-y p)})
        points)))

(defn convert-to-plottable
  [point-map]
  (set
   (map #(vector (:pos-x %) (:pos-y %)) point-map)))

(defn plot
  [points]
  (let [set-points (convert-to-plottable points)
        [[minx maxx] [miny maxy]] (bounds points)]
    (for [y (range miny (inc maxy))
          x (range minx (inc maxx))]
      (do (if (= x maxx)
            "\n"
            (if (set-points [x y])
              "#"
              "."))))))

(spit "solutions" (apply str (plot (tick testdata 10369))))
(bounds (tick testdata 10369))

(tick testdata)
