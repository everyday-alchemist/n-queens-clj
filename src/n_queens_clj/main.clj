(ns n-queens-clj.main
  (:require [clojure.pprint :refer [pprint]]))

(defn set-2d
  [board [col row] v]
  (let [r (get board row)]
    (assoc board row (assoc r col v))))

(defn down-diag
  [[col row]]
  (- col row))

(defn up-diag
  [[col row] n]
  (- (+ col row) n -1))

(defn valid-queen-pt?
  [occ row down up]
  (not
   (or (contains? (get occ :rows) row)
       (contains? (get occ :down-diag) down)
       (contains? (get occ :up-diag)  up))))

(defn n-queens
  [n board [col row] queens occ results]
  (let [curr-down-diag (down-diag [col row]) ; not worth splitting this into to lets to optimize
        curr-up-diag (up-diag [col row] n)]
    (if (and (< col n)
             (< row n))
      (if (valid-queen-pt? occ row curr-down-diag curr-up-diag)
        ;; this is a valid spot for a queen
        (if (< queens (dec n))
          ;; this queen will not be the nth queen
          ;; recurse, adding queen to [col row] moving to the first row of the next col and inc queen count
          (let [r (n-queens n (set-2d board [col row] "Q") [(inc col) 0] (inc queens) (-> occ
                                                                                        (update :rows conj row)
                                                                                        (update :down-diag conj curr-down-diag)
                                                                                        (update :up-diag conj curr-up-diag))
                            results)]

            ;; backtrack, using results from last recursive call
            (n-queens n board [col (inc row)] queens occ r))
          ;; this queen will complete a solution
          ;; add current state to results and backtrack by recursing without placing the queen 
          (n-queens n board [col (inc row)] queens occ
                  (conj results (set-2d board [col row] "Q"))))
        ;; this is not a valid spot for a queen - try next row
        (n-queens n board [col (inc row)] queens occ results))
      results)))

(defn init-board
  [n]
  (into [] (repeat n (into [] (repeat n nil)))))

(defn -main
  [arg]
  (let [n (Integer/parseInt arg)
        board (init-board n)
        r (time (n-queens n board [0 0] 0 {:rows #{} :down-diag #{} :up-diag #{}} []))]
    (pprint r)
    (println "Total number of solutions: " (count r))))
