(ns experiments.12-pixel-paintbrush
  (:require [clojure2d.core :as c]
            [clojure.core.reducers :as reducers]
            [common.utils :refer [update-state!]]
            [fastmath.core :as math]
            [fastmath.core :as m]
            [clojure2d.pixels :as p]))

(defrecord Pointer [x y direction speed active])

(defn next-pointer
  "Moves the pointer, taking to account speed"
  [{:keys [x y direction speed active] :as pointer} df fps]
  (let [[dx dy] direction
        d-pixels ( * df (* speed (/ 1 fps)))
        next-x (+ x (* dx d-pixels))
        next-y (+ y (* dy d-pixels))]
    (assoc pointer
           :x next-x
           :y next-y)))

(defn draw-trail
  [canvas trail]
  (doseq [[x y] trail]
    (-> canvas
        (c/set-color [255 255 255])
        (c/point x y))))

;;1px/60f
(defn draw [canvas window frame {:keys [last-frame trail] :as state}]
  (let [{:keys [pointer] :as w-state} (c/get-state window)
        {:keys [x y direciton speed active]} pointer
        df (- frame last-frame)]
    (-> canvas
        (c/set-background [0 0 0])

        ;;draw pointer
        (c/set-color [255 0 0])
        (c/ellipse x y 3 3)

        (draw-trail trail))
    (c/set-state! window (assoc w-state :pointer (next-pointer pointer df 60)))
    (-> state
        (assoc :last-frame frame)
        (update :trail #(cons [x y] %)))))

(let [w 800
      h 600
      fps 60
      name "night-lights"
      window (c/show-window {:window-name name
                             :canvas (c/canvas w h)
                             :always-on-top? true
                             :state {:speed-step 5
                                     :min-speed 10
                                     :max-speed 400
                                     :pointer (->Pointer (/ w 2) (/ h 2) [1 0] 10 false)}
                             :fps fps
                             :draw-state {:last-frame 0
                                          :trail '()}
                             :draw-fn #(draw %1 %2 %3 %4)})]

  (def direction-map {:left [-1 0]
                      :right [1 0]
                      :up [0 -1]
                      :down [0 1]})

  (def arrow-keys #{:left :right :up :down})

  (defn move-pointer
    [key {:keys [pointer speed-step min-speed max-speed] :as state}]
    (if (arrow-keys key)
      (let [{:keys [direction speed]} pointer
            next-direction (get direction-map key)
            same-direction? (= direction next-direction)
            opposite-direction? (reduce (partial = true)
                                        (map #(= (- %1) %2) direction next-direction))
            next-speed (cond
                         same-direction? (min max-speed (+ speed speed-step)) ;;speed-limits
                         opposite-direction? (max min-speed (- speed speed-step))
                         :else speed)]
        (update state :pointer #(assoc %
                                       :direction (if opposite-direction?
                                                    direction
                                                    next-direction)
                                       :speed next-speed)))
      state) )

  (defmethod c/key-event
    [name :key-pressed] [evt state]
    (move-pointer (c/key-code evt) state)
    ))
