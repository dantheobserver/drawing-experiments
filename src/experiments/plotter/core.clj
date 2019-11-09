(ns experiments.plotter.core
  (:require [com.rpl.specter
             :as
             sp
             :refer
             [ALL
              collect-one
              LAST
              multi-path
              multi-transform
              terminal
              terminal-val
              transform]]))

(defprotocol MovingEntity
  "Protocol to define a moving Object"
  (next-state [this state] "Return the next natural state for th object, with optional state"))

(defprotocol MovingEntityCollection
  "Protocol for collection of moving entities"
  (add [this entity])
  (update-items [this state]))

(defrecord PointRay [coord ray-coord ray-vec term-point]
  MovingEntity
  (next-state [this state]
    (let [next-coord (mapv + ray-coord ray-vec)]
      (assoc this :ray-coord next-coord))))

;; (deftype PointRayCollection [vector]
;;   clojure.lang.IPersistentVector
;;   clojure.lang.IFn)
