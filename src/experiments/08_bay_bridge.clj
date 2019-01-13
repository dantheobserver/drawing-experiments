(ns experiments.08-bay-bridge
  (:require [fastmath.core :as math]
            [fastmath.random :as r]
            [clojure2d.core :as c]
            [clojure2d.color :as clr]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]))

(def pi math/PI)
(def two-pi math/TWO_PI)

(def state (atom {:bridge/height 200
                  :bridge/width 300
                  :bridge/section-width 80
                  :beam/count 100}))

(defn plot-y
  "Finds the y position for a beam"
  [x width max-height]
  (-> (* two-pi x)
      (/ width)
      (+ (/ (* 3 pi) 2))
      math/sin
      inc
      (/ 2)
      (* max-height)))

(defn- next-beam
  [step width max-height]
  (fn [[x0 _]]
    [(+ step x0)
     (plot-y x0 width max-height)]))

(defn bridge-section
  [width max-height beam-count]
  (let [step (/ width beam-count)]
    (iterate (next-beam step width max-height) [0 0])))

(defn draw-bridge
  [canvas window frame {:bridge/keys [height width]
                        :beam/keys [count]}]
  (c/set-background canvas [0 0 0])
  (let [sections (take-while
                  (fn [[x _]](< x width))
                  (bridge-section width height count))]
    (doseq [[x1 y1] sections]
      (-> canvas
          (c/set-color [255 255 255])
          (c/set-stroke 2)
          (c/line [x1 300] [x1 (- 300 y1)]))))
  @state)

(defn draw-proxy
  [canvas window frame state]
  (draw-bridge canvas window frame state))

(def window
  (c/show-window
   {:canvas (c/canvas 800 300)
    :window-name "bay bridge"
    :draw-fn draw-proxy
    :draw-state @state}))
