(ns basics.vector
  (:require [fastmath.core :as math]))

(defn *v
  "multiplies vector by scalar"
  [vec s]
  (mapv #(* s %) vec))

(defn +v [v1 v2]
  (mapv + v1 v2))

(defn -v
  ([vec] (mapv - vec))
  ([v1 v2] (mapv - v1 v2)))

(defn magnitude
  "magnitude of vector"
  [vec]
  (math/sqrt (reduce #(+ %1 (* %2 %2)) 0 vec)))

(defn normalized
  "gets unit vector"
  [vec]
  (*v vec (/ 1 (magnitude vec))))

