(ns experiments.03-deep-rain
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; TODO: Ideas
;; Drops
;; * Rain will have a z coord which control its depth
;; * Transparency and size x/(abs z),
;; * Will accellerate consistently, but have different `weight`
;; * Intensity ebbs and flows
;; * Create concentrations at random points.
;; * Affected by wind
;; Scene
;; * Cause thunder on click-event
;; * Can see 'ceiling' of sky for further elements,
;; Bonus
;; * See if you can place a moving object that will get drawn
;;   behind the rain droplets that are closer than it is.

(def fps 30)

(defn update-drops [drops]
  (map (fn [{:keys [speed accel] :as drop}]
         (-> drop
             (update :y + speed)
             (update :speed + accel)))
       drops))

(defn update-state [state]
  (-> state
      (update :drops update-drops)))

(defn render-drop [canvas {:keys [x y z] :as drop}]
  (-> canvas
      (c/set-color [255 255 255 (/ 255 0.5)])
      (c/rect x y 10 10)))

(defn draw-fn [canvas window ^long frame {:keys [bg-color drops] :as state}]
  (let [t (/ frame ^long (:fps window))]
    (c/set-background canvas bg-color)
    (doseq [drop drops]
      (-> canvas
          (render-drop drop))))
  (update-state state))

(let [window-name "rain-scene"
      window-size [500 500]
      max-drops 100
      bg-color "#08061c"
      global-state {:wind 0}
      accelleration 0.5]
  (c/show-window {:canvas (apply c/canvas window-size)
                  :window-name window-name
                  :draw-fn draw-fn
                  :draw-state {:bg-color bg-color
                               :drops (list {:x 50 :y 2 :z 0 :speed 0 :accel 0.5})}' ;;(draw-state max-drops)
                  :state global-state
                  :fps fps}))
