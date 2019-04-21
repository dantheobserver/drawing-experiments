(ns experiments.12-pixel-paintbrush
  (:require [clojure2d.core :as c]
            [clojure.core.reducers :as reducers]
            [fastmath.core :as math]))

(defrecord Pointer [x y direction speed active])

(defn next-pointer
  "Moves the pointer, taking to account speed"
  [{:keys [x y direction speed active] :as pointer} frame fps]
  (let [[dx dy] direction
        norm-time (mod frame fps)
        ;; norm-time (/ (rem frame fps) fps)
        #_(double (/ (rem 120 60) 60))
        d-pixels (if (zero? norm-time)
                   speed
                   0)]
    (assoc pointer
           :x (+ x (* dx d-pixels))
           :y (+ y (* dy d-pixels)))))

(defn draw [canvas window frame state]
  (let [pointer (:pointer (c/get-state window))
        {:keys [x y direciton speed active]} pointer]
    (-> canvas
        (c/set-background [0 0 0])
        (c/set-color [255 0 0])
        (c/ellipse x y 3 3))
    (c/set-state! window (assoc state :pointer (next-pointer pointer frame (:fps window)))))
  state)

(let [w 800
      h 600
      name "night-lights"
      window (c/show-window {:window-name name
                             :canvas (c/canvas w h)
                             :always-on-top? true
                             :state {:pointer (->Pointer (/ w 2) (/ h 2) [1 0] 1 false)}
                             :fps 60
                             ;;:draw-state (get-draw-state w h)
                             :draw-fn #(draw %1 %2 %3 %4)})]

  (def diretion-map {:left [-1 0]
                     :right [1 0]
                     :up [0 -1]
                     :down [0 1]})

  (defmethod c/key-event [name :key-pressed] [evt {:keys [pointer] :as state}]
    (let [{:keys [direction speed]} pointer
          next-direction (get direction-map (c/key-code evt))
          same-direction? (= direction next-direction)
          opposite-direction? (reduce (partial = true)
                                      (map #(= (- %1) %2) direction next-direction))
          next-speed (cond
                       same-direction? (min 10 (+ speed 0.5)) ;;speed-limits
                       opposite-direction? (max 1 (- speed 0.5)) :else speed)]
      (c/set-state! window (update state :pointer #(assoc %
                                                          :direction (if opposite-direction?
                                                                       direction
                                                                       next-direction)
                                                          :speed next-speed))))))
