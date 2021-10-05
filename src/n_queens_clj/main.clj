(ns n-queens-clj.main
  (:require [clojure.pprint :refer [pprint]]))

(defn get-2d
  [board [col row]]
  (-> board
      (get row)
      (get col)))

(defn set-2d
  [board [col row] v]
  (let [r (get board row)]
    (assoc board row (assoc r col v))))

(defn down-diag-start
  "takes point and returns the starting point of the
  top-left to bottom-right diagonal containing the point"
  [[col row]]
  (let [diag (- col row)]
    (if (< diag 0)
      [0 (* diag -1)]
      [diag 0])))

(defn up-diag-start
  "takes point and length of board and returns the starting point 
  of the bottom-left to top-right diagonal containing the point"
  [[col row] n]
  (let [diag (- (+ col row) n -1)]
    (if (< diag 0)
      [0 (+ 3 diag)]
      [diag 3])))

(defn down-diag-ind
  "takes a top-left to bottom-right diagonal 
  starting point and returns its indices"
  [[start-col start-row] n]
  (loop [col start-col
         row start-row
         ind []]
    (if (or (>= col n)
            (>= row n))
      ind
      (recur (inc col) (inc row) (conj ind [col row])))))

(defn up-diag-ind
  "takes a bottom-left to to top-right diagonal
  starting point and returns its indices"
  [[start-col start-row] n]
  (loop [col start-col
         row start-row
         ind []]
    (if (or (>= col n)
            (< row 0))
      ind
      (recur (inc col) (dec row) (conj ind [col row])))))

(defn unocc-down-diag?
  "returns true if the top-left to botom-right diagonal contains only nil values"
  [board [col row]]
  (let [n (count board)
        start (down-diag-start [col row])
        ind (down-diag-ind start n)]
    (every? nil? (map #(get-2d board %) ind))))

(defn unocc-up-diag?
  "returns true if the bottom-left to top-right diagonal contains only nil values"
  [board [col row]]
  (let [n (count board)
        start (up-diag-start [col row] n)
        ind (up-diag-ind start n)]
    (every? nil? (map #(get-2d board %) ind))))

(defn unocc-row?
  "returns true if the row of the board only contains nil values"
  [board row]
  (every? nil? (get board row)))

(defn unocc-col?
  "returns true if the row of the board only contains nil values"
  [board col]
  (every? nil? (map #(get % col) board)))

(defn valid-queen-pt?
  "returns true if the point is a valid place for a queen"
  [board [col row]]
  (and (unocc-row? board row)
       (unocc-col? board col)
       (unocc-down-diag? board [col row])
       (unocc-up-diag? board [col row])))


(def results (atom []))

(defn n-queens 
  [board [col row] queens]
;  (pprint board)
;  (println (str [col row]))
;  (println (str "Queens: " queens))
;  (println "------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
  (when (= queens 8)
    (println "solution")
    (pprint board))
  (let [n (count board)]
    (when (and (< col n)
             (< row n))
      (if (valid-queen-pt? board [col row])
        ;; this is a valid spot for a queen
        (if (< queens n)
          ;; this queen will not be the nth queen
          (do
            ;; recurse, adding queen to [col row] moving to the first row of the next col and inc queen count
            (n-queens (set-2d board [col row] "Q") [(inc col) 0] (inc queens))
            ;; backtrack, trying next row without adding a queen
            (n-queens board [col (inc row)] queens))
          ;; this queen will complete a solution
          ;; backtrack by recursing without placing the queen 
          (do
            (swap! results conj (set-2d board [col row] "Q"))
            (n-queens board [col (inc row)] queens)))
        ;; this is not a valid spot for a queen - try next row
        (n-queens board [col (inc row)] queens)))))

(defn -main 
  []
  (let [board [[nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]
               [nil nil nil nil nil nil nil nil]]]
    (n-queens board [0 0] 0))
  (pprint @results))

;; FIXME: something is broken with the up-diag functions
(up-diag-ind (up-diag-start [1 6] 8) 8)
(up-diag-start [1 6] 8)
