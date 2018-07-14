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
(defrecord RainDrop [x y z speed accel])

(defn- new-droplet [max-x max-y max-z]
  (->RainDrop
   (r/frand max-x) max-y (r/frand max-z)
   0.0
   (r/frand 0.3 2)))

(defn- visible?
  [{:keys [^long y]} ^long height]
  (< y height))

(defn update-drop [{:keys [window-size max-depth]}]
  (fn [{:keys [speed accel] :as drop}]
    (let [[width height] window-size]
      (if (visible? drop height)
        (-> drop
            (update :y + speed)
            (update :speed + accel))
        (new-droplet width -50 max-depth)))))

(defn render-drop
  [canvas {:keys [x y ^long z] :as drop} {:keys [^long max-depth]}]
  (let [alpha (* (/ z max-depth) 255)]
    (-> canvas
        (c/set-color [255 255 255 alpha])
        (c/rect x y 2 10))))

(defn draw-fn [canvas window ^long frame {:keys [bg-color drops] :as state}]
  (let [{:keys [fps w h]} window
        t (/ frame ^long fps)
        global-state (c/get-state window)]
    (c/set-background canvas bg-color)
    (doseq [drop drops]
      (-> canvas
          (render-drop drop global-state)))
    ;; Update state
    (-> state
        (update :drops #(map (update-drop global-state) %)))))

(defn- initial-state [bg-color {:keys [window-size max-depth]}]
  {:bg-color bg-color
   :drops (repeatedly 1000 #(new-droplet (window-size 0) -200 max-depth))})

(let [window-name "rain-scene"
      window-size [800 600]
      fps 30
      bg-color "#08061c"
      canvas (apply c/canvas window-size)
      global-state {:wind 0
                    :window-size window-size
                    :max-drops 100
                    :max-depth 30}]
  (c/show-window {:canvas canvas
                  :window-name window-name
                  :draw-fn draw-fn
                  :draw-state (initial-state bg-color global-state)
                  :state global-state
                  :fps fps}))
