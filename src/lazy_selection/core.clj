(ns lazy-selection.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(defn random_array
  "Returns a random permutation of an array of n unique elements. In the
  range 1..inf, element i is chosen with probability p (defaults to
  1/2)."
  [n & {:keys [p] :or {p 0.5}}]
  (shuffle (take n (random-sample p (range)))))

(defn rank_of
  "Returns the rank of element e in array a. Takes O(n). Assumes element
  is in array, otherwise it returns the rank it would have if it was"
  [i A]
  (reduce (fn [e1 e2] (if (< e2 i) (+ e1 1) e1)) 1 A))

(defn R
  "Returns R, which are the randomly sampled n^{3/4} elements of array
  S"
  [S]
  (let [n (count S)
        sample (math/ceil (math/expt n (/ 3 4)))]
    (take sample (shuffle S))))

(defn quick_sort
  "Modified version of quick sort that includes information about the
  number of comparisons"
  [[pivot & coll]]
  (when pivot
    (let [greater? #(> % pivot)]
      (lazy-cat (quick_sort (remove greater? coll))
                [pivot]
                [{:comp (count coll)}]
                (quick_sort (filter greater? coll))))))

(defn sorted
  "Returns only the list of sorted elements"
  [quicksort_output]
  (remove map? quicksort_output))

(defn count_comp
  "Returns the number of comparisons performed in a quick sort"
  [quicksort_output]
  (reduce (fn [e1 e2] (if (map? e2) (+ e1 (:comp e2)) e1))
          0 quicksort_output))

(defn lazy_select
  "Returns the kth smallest element in S"
  [S k & {:keys [comps] :or {comps 0}}]
  (let [n (count S)
        R (R S)
        q_sort_R (quick_sort R)
        R_sorted (sorted q_sort_R)
        R_comp (+ comps (count_comp q_sort_R))
        x (* k (math/expt n (/ -1 4)))
        l (max (math/floor (- x (math/sqrt n))) 1)
        h (min (math/ceil (+ x (math/sqrt n))) (math/floor (math/expt n (/ 3 4))))
        a (nth R_sorted l)
        b (nth R_sorted h)
        ra (rank_of a S)
        rb (rank_of b S)
        P (cond
            (< k (math/expt n (/ 1 4))) (filter #(<= % b) S)
            (> k (- n (math/expt n (/ 1 4)))) (filter #(>= % a) S)
            :else (filter #(and (>= % a) (<= % b)) S))
        q_sort_P (quick_sort P)
        P_sorted (sorted q_sort_P)
        P_comp (count_comp q_sort_P)]
    (if (and (and (>= k ra) (<= k rb))
             (<= (count P) (+ (* (math/expt n (/ 3 4)) 4) 2)))
      {:elem (nth P_sorted (+ (- k ra) 1))
       :comps (+ R_comp P_comp)}
      (lazy_select S k :comps (+ R_comp P_comp)))))


(defn -main []
  (lazy_select (shuffle (range 100000)) 500))
