(ns advent.day-11)

(def serial 8561)

(def grid
  (for [x (range 1 301)
        y (range 1 301)]
    [x y]))

(defn powerlevel
  "Calculate powerlevel of a single cell"
  [[x y] serial]
  (let [magic (-> (+ x 10)
                  (* y)
                  (+ serial)
                  (* (+ x 10)))
        magic-str (str magic)
        digit (str (get magic-str (- (count magic-str) 3)))]
    (if (nil? digit)
      -5
      (- (read-string digit) 5))))

(defn powergrid
  [grid serial]
  (zipmap grid (map powerlevel grid (repeat serial))))

(def possible-squares
  (for [x (range 1 299)
        y (range 1 299)]
    [x y]))

(defn powersquare
  [grid powermap]
  (into
   {}
   (map (fn [[x y]]
          [[x y]
           (+ (get powermap [x y]) (get powermap [(inc x) y]) (get powermap [(+ 2 x) y])
              (get powermap [x (inc y)]) (get powermap [(inc x) (inc y)]) (get powermap [(+ 2 x) (inc y)])
              (get powermap [x (+ 2 y)]) (get powermap [(inc x) (+ 2 y)]) (get powermap [(+ 2 x) (+ 2 y)]))])
        grid)))

(defn flexible-powersquare
  [size powermap]
  (let [maxcoord (- 301 (dec size))
        search-space (for [x (range 1 maxcoord)
                           y (range 1 maxcoord)]
                       [x y])]
    (println "Building size: " size)
    (into {}
          (map (fn [[x y]]
                 (let [square (for [sx (range x (+ x size))
                                    sy (range y (+ y size))]
                                [sx sy])]
                   [[x y]
                    (reduce #(+ %1 (get powermap %2)) 0 square)]))
               search-space))))


(defn part-1
  [grid serial possible-squares]
  (->> (powergrid grid serial)
       (powersquare possible-squares)
       (apply max-key val)
       key))

#_(time (part-1 grid serial possible-squares))
;; [21 37]




(defn part-2
  [grid serial]
  (let [powermap (powergrid grid serial)]
    (map
     (fn [size]
       [size (apply max-key val (flexible-powersquare size powermap))])
     (range 1 301))))

#_(time (part-2 grid serial))
