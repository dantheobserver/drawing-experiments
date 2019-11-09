(ns basics.matrix
  (:require [fastmath.core :as math]
            [basics.vector]))

;; Row major
(defn matrix
  [& rows]
  (into [] rows))

(defn m-idx
  "Acces element at row `i` and column `j`"
  [matrix i j]
  (-> matrix
      (get i)
      (get j)))

(letfn [(-diagonal
          [matrix row-fn]
          (loop [[row & others] matrix
                 c 0
                 res []]
            (if row
              (let [new-row (into [] (map-indexed (fn [i v] (row-fn i v c)) row))]
                (recur others (inc c)
                       (conj res new-row)))
              res)))]

  (defn diagonal
    [matrix]
    (-diagonal matrix (fn [i v c] (if (= i c) v 0))))

  (defn off-diagonal
    [matrix]
    (-diagonal matrix (fn [i v c] (if (= i c) 0 v)))))

(defn transpose
  [matrix]
  (let [rows (count matrix)]
    (->> matrix
         (apply interleave)
         (partition rows)
         (mapv #(into [] %)))))

(defn symmetric?
  [matrix]
  (if (= (count matrix)
         (count (first matrix)))
    (= matrix (transpose matrix))
    false))


;; Column major
#(matrix3
   1 2 3
   4 5 6
   7 8 9)
;; =>
[1 4 7]
[2 5 8]
[3 6 9]
(defn matrix3
  "Creates a 3d matrix of `elements` stored in
  column ajor order"
  [& elements]
  (if (= 0 (rem (count elements) 3))
    (loop [m-rows (partition 3 elements) 
           matrix []]
      (if (every? empty? m-rows)
        matrix
        (recur
          (map rest m-rows)
          (conj matrix (mapv first m-rows)))))))

(defn m3-get
  [m3 col row]
  (-> m3
      (get col)
      (get row)))

;; matrix
;; [1 2 3]
;; [4 5 6]
;; transpose
;; [1 4]
;; [2 5]
;; [3 6]

;;0, 0 => [1 2 3]

(comment
  (let [m (matrix
            [1 2 3]
            [4 5 6]
            [7 8 9])]
    (diagonal m)
    (off-diagonal m)
    (transpose m)
    (symmetric? m)
    (m-idx m 1 1))

  (symmetric? [[1 5 -3]
               [5 4 2]
               [-3 2 0]]) ;;
  
  )
