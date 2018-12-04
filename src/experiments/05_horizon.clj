(ns experiments.05-horizon
  (:require [clojure2d.core :as c]
            [fastmath.core :as math]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def env {:dimensions [500 500]
          :vanishing-point 50})

(defn half [x] (/ x 2))

(defn center-coords [[x y]] [(half x) (half y)])

(defprotocol Project
  (project [_])
  (points [_]))

(defrecord FloorLine [pos width])

(defn project
  "Takes an element with an x, y and z coordinate
  and returns its associated projection"
  [{:keys [pos]} dimensions]
  (let [[x y z] pos
        [_ h] (center-coords dimensions)
        diff-from-center (- y h)]
    ))


(defn calc-width [{:keys [width z]}])
