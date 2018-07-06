(ns experiments.02-cloud-trail
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas-size [500 500])
(def x-range 50)
(def y-range 100)
(def spawn-pos (atom [250 250])) ;; TODO: Update from mouse cursor

;;; Cloud trail functions
(defn- trail-visible? [{:keys [^long y ^long size]}]
  (or (< 0.0 size) (< y ^long (canvas-size 1))))
(defn- rand-x [^long x] (r/frand (- x ^long x-range) (+ x ^long x-range)))
(defn- rand-y [^long y] (r/frand (- y ^long y-range) (+ y ^long y-range)))
(defn- rand-size [] (r/frand 60 100))
(defn- rand-rate [] (r/frand 1 10))

(defn- next-part [{:keys [^long x ^long y ^long size ^long rate] :as part}]
  (if (trail-visible? part)
    ;; Shrink and jostle trail
    (-> part
        (update :size - (r/frand 1 10))
        (update :y + (r/frand 1 10))
        (update :x + (r/frand -6 6)))
    ;; Respawn wherever the spawn pos is
    (let [[^long x ^long y] @spawn-pos]
      (-> part
          (assoc :x (rand-x x)) ; x pos
          (assoc :y (rand-y y)) ; y pos
          (assoc :size (rand-size)) ; size
          (assoc :rate (rand-rate)))))) ; rate

(defn setup [canvas window]
  (let [[^long x ^long y] @spawn-pos]
    (repeatedly 100 #(hash-map
                      :x (rand-x x)
                      :y (rand-y y)
                      :size (rand-size)
                      :rate (rand-rate)))))

(defn draw [canvas window frame trail-parts]
  (c/set-background canvas :black)
  (doseq [{:keys [x y size] :as cur-part} trail-parts]
    (-> canvas
        (c/ellipse x y size size)
        (c/set-color [255 255 255 (r/irand 100 255)])))
  (map next-part trail-parts))

(defn run []
  (let [[w h] canvas-size]
    (c/with-canvas [canvas (c/canvas w h)]
      (c/show-window
       {:canvas canvas
        :fps 30
        :draw-fn draw
        :setup setup}))))

(run)
