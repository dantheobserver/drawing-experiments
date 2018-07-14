(ns experiments.02-cloud-trail
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def canvas-size [500 500])
(def x-range 60)
(def y-range 60)

;;; Cloud trail functions
(defn- trail-visible? [{:keys [^long size]}]
  (< 0.0 size))
(defn- rand-transp [] [255 255 255 (r/irand 100 200)])
(defn- rand-x [^long x] (r/frand (- x ^long x-range) (+ x ^long x-range)))
(defn- rand-y [^long y] (r/frand (- y ^long y-range) (+ y ^long y-range)))
(defn- rand-size [] (r/frand 60 100))
(defn- rand-rate [] (r/frand 1 2))

(defn- next-part [{:keys [^long x ^long y ^long size ^long rate] :as part} spawn-pos]
  (if (trail-visible? part)
    ;; Shrink and jostle trail
    (let [x-rng (/ size 6)]
      (-> part
          (update :size - 2)
          (update :y + 10)))
    ;; Respawn wherever the spawn pos is
    (let [[^long x ^long y] spawn-pos]
      {:x (rand-x x) ; x pos
       :y (rand-y y) ; y pos
       :size (rand-size) ; size
       :rate (rand-rate)
       :color (rand-transp)}))) ; rate

(defn draw-state [spawn-pos]
  (let [[^long x ^long y] spawn-pos]
    (repeatedly 100 #(hash-map
                      :x (rand-x x)
                      :y (rand-y y)
                      :size (r/frand 10 100)
                      :rate (rand-rate)
                      :color (rand-transp)))))

(defn draw [canvas window frame_ trails]
  (c/set-background canvas "#5474d3")
  (doseq [{:keys [x y size color] :as cur-part} trails]
    (-> canvas
        (c/set-color color)
        (c/ellipse x y size size)))
  (let [spawn-pos (-> window c/get-state :spawn-pos)]
    (map #(next-part % spawn-pos) trails)))

(def window-name "cloud-trail")
(defn run []
  (let [[w h] canvas-size
        spawn-pos [250 250]]
    (c/with-canvas [canvas (c/canvas w h)]
      (c/show-window
       {:canvas canvas
        :window-name window-name
        :fps 30
        :draw-fn draw
        :state {:spawn-pos spawn-pos}
        :draw-state (draw-state spawn-pos)}))))

(defmethod c/mouse-event [window-name :mouse-moved] [e state]
  (let [new-pos ((juxt c/mouse-x c/mouse-y) e)]
    (assoc state :spawn-pos new-pos)))
