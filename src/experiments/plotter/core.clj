(ns experiments.plotter.core
  (:require [com.rpl.specter
             :as s
             :refer-macros [transform select]
             :refer [ALL]
             ]))

(defprotocol MovingEntity
  "Protocol to define a moving Object"
  (next-state [this state] "Return the next natural state for th object, with optional state"))

(defrecord PointRay [coord ray-coord ray-vec term-point]
  MovingEntity
  (next-state [this state]
    (let [next-coord (mapv + ray-coord ray-vec)]
      (assoc this :ray-coord next-coord))))

#_(defn )

