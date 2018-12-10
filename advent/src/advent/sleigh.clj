(ns advent.sleigh
  (:require [clojure.string :as str]))

(defn- parse-action-line
  [line]
  (rest (re-find #"^Step ([A-Z]) must be finished before step ([A-Z]) can begin\.$" line)))

(defn build-dag
  [instructions]
  (let [lines (str/split instructions #"\n")
        tuples (map parse-action-line lines)]
    (reduce (fn [graph [k v]]
              (update graph v conj k)) {}
            tuples)))

(def upper-alpha
  (map (comp str char) (range 65 91)))

(defn- complete-dag
  [dag]
  (reduce #(update %1 %2 identity) dag upper-alpha))

(defn- next-candidates
  [dag]
  (->>
   (map #(when (empty? (val %)) (key %)) dag)
   (remove nil?)
   sort))

(defn- remove-satisfied
  [node dag]
  (dissoc
   (->> dag
        (map (fn [[k v]] [k (remove #{node} v)]))
        (into {}))
   node))

(defn topological-sort
  [dag]
  (loop [order ""
         rest-dag dag
         queue (next-candidates rest-dag)]
    (if (empty? rest-dag)
      order
      (let [candidate (first queue)
            new-dag (remove-satisfied candidate rest-dag)]
        (recur (str order candidate) new-dag (next-candidates new-dag))))))

(defn part-1
  [instructions]
  (-> instructions
      build-dag
      complete-dag
      topological-sort))

#_(part-1 instructions)
;;BCEFLDMQTXHZGKIASVJYORPUWN
;; ;; ;; ;; ;; Part 2 ;; ;; ;; ;; ;;
(defn- filter-finished-workers
  [workers]
  (filter #(< 0 (:time-left %)) workers))

(defn- completed-projects
  [workers]
  (map :project
       (filter #(= 0 (:time-left %)) workers)))

(defn- update-clocks
  [workers]
  (map #(update % :time-left dec) workers))

(defn- time-for
  [node]
  (+ (int (first node)) -4))

(defn- in-progress
  [workers]
  (into #{} (map :project workers)))


(defn assign-workers
  "Assume a maximum of 5 separate workers."
  [workers queue]
  (let [limit 5
        new-workers (remove nil? (map (fn [node] (when-not ((in-progress workers) node)
                                                  {:project node
                                                   :time-left (time-for node)}))
                                      (take limit queue)))]
    (concat workers (take (- limit (count workers)) new-workers))))

(defn remove-all-satisfied
  [dag completed]
  (reduce #(remove-satisfied %2 %1) dag completed))

(defn- stupid-fullify
  [dag]
  (let [all-letters (into #{} (flatten (concat (keys dag) (vals dag))))]
    (reduce (fn [dag key]
              (if (contains? dag key)
                dag
                (assoc dag key [])))
            dag all-letters)))

(defn teamwork
  ([dag]
   (teamwork dag #{}))
  ([dag not-in]
   (loop [time -1
          rest-dag dag
          completed not-in
          queue (next-candidates rest-dag)
          workers []]
     (if (and (empty? rest-dag) (empty? workers))
       time
       (let [temp-workers (update-clocks workers)
             new-completed (set (concat completed (completed-projects temp-workers)))
             new-dag (remove-all-satisfied rest-dag new-completed)
             new-queue (next-candidates new-dag)
             new-workers (assign-workers (filter-finished-workers temp-workers) new-queue)]
         (recur (inc time) new-dag new-completed new-queue new-workers))))))

(defn part-2
  [instructions]
  (-> instructions
      build-dag
      stupid-fullify
      teamwork))

#_(part-2 instructions)
;; 987

(def small-instructions
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

#_(part-2 small-instructions)
;;999
(def instructions
  "Step L must be finished before step M can begin.
Step B must be finished before step S can begin.
Step F must be finished before step M can begin.
Step C must be finished before step P can begin.
Step D must be finished before step V can begin.
Step T must be finished before step J can begin.
Step E must be finished before step S can begin.
Step Z must be finished before step V can begin.
Step X must be finished before step U can begin.
Step G must be finished before step I can begin.
Step V must be finished before step W can begin.
Step H must be finished before step O can begin.
Step M must be finished before step S can begin.
Step K must be finished before step I can begin.
Step I must be finished before step J can begin.
Step A must be finished before step W can begin.
Step J must be finished before step R can begin.
Step Q must be finished before step S can begin.
Step Y must be finished before step R can begin.
Step O must be finished before step W can begin.
Step R must be finished before step U can begin.
Step P must be finished before step U can begin.
Step S must be finished before step W can begin.
Step U must be finished before step N can begin.
Step W must be finished before step N can begin.
Step P must be finished before step W can begin.
Step J must be finished before step W can begin.
Step F must be finished before step G can begin.
Step U must be finished before step W can begin.
Step Y must be finished before step P can begin.
Step Z must be finished before step I can begin.
Step R must be finished before step W can begin.
Step T must be finished before step X can begin.
Step Q must be finished before step R can begin.
Step B must be finished before step P can begin.
Step Z must be finished before step U can begin.
Step H must be finished before step Y can begin.
Step G must be finished before step A can begin.
Step O must be finished before step P can begin.
Step F must be finished before step D can begin.
Step F must be finished before step Q can begin.
Step T must be finished before step U can begin.
Step I must be finished before step O can begin.
Step K must be finished before step R can begin.
Step E must be finished before step J can begin.
Step Z must be finished before step G can begin.
Step Y must be finished before step W can begin.
Step L must be finished before step V can begin.
Step E must be finished before step X can begin.
Step E must be finished before step U can begin.
Step A must be finished before step N can begin.
Step G must be finished before step N can begin.
Step B must be finished before step C can begin.
Step M must be finished before step U can begin.
Step G must be finished before step R can begin.
Step R must be finished before step N can begin.
Step M must be finished before step K can begin.
Step C must be finished before step E can begin.
Step B must be finished before step U can begin.
Step J must be finished before step Y can begin.
Step X must be finished before step H can begin.
Step E must be finished before step W can begin.
Step A must be finished before step Y can begin.
Step I must be finished before step A can begin.
Step D must be finished before step W can begin.
Step B must be finished before step I can begin.
Step H must be finished before step P can begin.
Step A must be finished before step S can begin.
Step P must be finished before step N can begin.
Step V must be finished before step J can begin.
Step L must be finished before step D can begin.
Step C must be finished before step R can begin.
Step Z must be finished before step Y can begin.
Step F must be finished before step S can begin.
Step O must be finished before step N can begin.
Step X must be finished before step R can begin.
Step E must be finished before step Q can begin.
Step L must be finished before step Z can begin.
Step D must be finished before step O can begin.
Step Y must be finished before step O can begin.
Step S must be finished before step N can begin.
Step D must be finished before step X can begin.
Step T must be finished before step A can begin.
Step Q must be finished before step Y can begin.
Step K must be finished before step J can begin.
Step C must be finished before step S can begin.
Step M must be finished before step P can begin.
Step O must be finished before step R can begin.
Step E must be finished before step Y can begin.
Step V must be finished before step O can begin.
Step D must be finished before step Q can begin.
Step D must be finished before step J can begin.
Step D must be finished before step R can begin.
Step R must be finished before step P can begin.
Step X must be finished before step O can begin.
Step C must be finished before step K can begin.
Step L must be finished before step R can begin.
Step Q must be finished before step U can begin.
Step G must be finished before step K can begin.
Step I must be finished before step S can begin.
Step L must be finished before step X can begin.")

