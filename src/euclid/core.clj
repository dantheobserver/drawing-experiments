(ns euclid.core
  (:require [fastmath.core :as math]
            [clojure2d.core :as c]
            [clojure2d.color :as color]
            [clojure2d.pixels :as p]
            [clojure.core.reducers :as r]))

(def pi 3.14159265)

(defn calc-x
  "Calculates the x value on a circle
  with a `radius` and offset by `x-offset`"
  [x-offset radius rad]

  (+ x-offset (* radius (math/cos rad))))

(defn calc-y
  "Calculates the y value on a circle
  with a `radius` and offset by `y-offset`"
  [y-offset radius rad]
  (+ y-offset (* radius (math/sin rad))))

(defn orbit-seq
  "Returns lazy-seq of positions of an orbiting point.
  Providing a positioned `circle` and a rate of change `rad-delta`
  with optional `start-rad`, a lazy sequence of coordinates around
  the circle will e created."
  ([{:keys [x y r] :as circle} rad-delta] (orbit-seq circle rad-delta 0))
  ([{:keys [x y r] :as circle} rad-delta start-rad]
   (iterate (fn [point]
              (let [next-rad (+ (:rad point) rad-delta)]
                (assoc point
                       :rad next-rad
                       :x (calc-x x r next-rad)
                       :y (calc-y y r next-rad))))
            {:rad start-rad
             :x (calc-x x r start-rad)
             :y (calc-y y r start-rad)})))

(defn draw
  [canvas window frame {:keys [circles orbits] :as state}]
  (c/set-background canvas :white)
  (doseq [{:keys [x y r]} circles
          :let [size (* 2 r)]]
    (-> canvas
        (c/set-color :blue)
        (c/set-stroke 1 (:round c/stroke-caps))
        (c/ellipse x y size size true)))
  (doseq [[cur-orbit] orbits
          :let [{:keys [x y]} cur-orbit]]
    (-> canvas
        (c/set-color :red)
        (c/ellipse x y 10 10)))
  (update state :orbits #(map rest %)))

(let [state {:window-name "eq-triangle"
             :window-size [500 500]
             :fps 30}
      [mx my] (into [] (map #(/ % 2)) (:window-size state))
      circles [{:x (+ mx 50) :y my :r 100}
               {:x (- mx 50) :y my :r 100}]
      orbits [(orbit-seq (circles 0) -0.2)
              (orbit-seq (circles 1) 0.2)]
      draw-state {:circles circles
                  :orbits orbits}]
  (c/show-window {:canvas (apply c/canvas (:window-size state))
                  :window-name (:window-name state)
                  :draw-fn draw
                  :state state
                  :setup (fn [canvas window]
                           (c/set-background canvas :white)
                           draw-state)
                  :fps (:fps state)}))
;; ex
;; (circle 0 0 2 8) ; Should give a circle with 8 points
