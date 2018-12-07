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

(part-1 instructions)

(topological-sort (complete-dag (build-dag instructions)))
;;BCEFLDMQTXHZGKIASVJYORPUWN

(build-dag instructions)

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

