(ns experiments.02-cloud-trail
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas-size [500 500])

(defn- trail-visible? [{:keys [^long y ^long size]}]
  (or (< 0.0 size)
      (< ^long (canvas-size 1) y)))

(defn- next-piece [{:keys [x y size rate] :as piece}]
  (-> piece
      (update :size - (r/frand 1 4))
      (update :y + (r/frand 1 6))
      (update :x + (r/frand -6 6))))

(defn cloud-trail
  "A lazy collection that shrinks, jostles and disappears."
  [x y size rate]
  (let [initial {:x x, :y y, :size size, :rate rate}]
    (take-while trail-visible?
                (iterate next-piece initial))))

(defn setup [canvas window]
  (cloud-trail 250 30 100 1))

(defn draw [canvas window frame [cur-trail & rem-trail]]
  (if-let [{:keys [x y size]} cur-trail]
    (do
      (c/set-background canvas :black)
      (-> canvas
          (c/set-color :white)
          (c/ellipse x y size size)))
    (c/set-background canvas :black))
  rem-trail)

(defn run []
  (let [[w h] canvas-size]
    (c/with-canvas [canvas (c/canvas w h)]
      (c/show-window
       {:canvas canvas
        :fps 30
        :draw-fn draw
        :setup setup}))))
