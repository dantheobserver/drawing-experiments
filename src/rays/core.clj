(ns rays.core
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as math]))

(defn- mouse-pos [e]
  ((juxt c/mouse-x c/mouse-y) e))

(defn- state-val [window key]
  (key (c/get-state window)))

(defn rad-offset
  [[x1 y1 :as p1] [x2 y2 :as p2] r]
  (let [x (- x2 x1)
        y (- y2 y1)
        c (math/hypot-sqrt x y)
        x' (/ (* x r) c)
        y' (/ (* y r) c)]
    [x' y']))

;; Drawing ray lines.
(defn draw [canvas window frame state]
  (c/set-background canvas 0 0 0)
  (let [[rx ry :as light-origin] (state-val window :ray-pos)
        light-radius (state-val window :light-radius)
        {:keys [w h]} canvas
        [px py :as planet-origin] [500 500]
        p-width 200
        p-rad (/ p-width 2)
        [dx dy] (rad-offset planet-origin light-origin p-rad)]
    (-> canvas
        ;; object
        (c/set-color (:planet-obj palette))
        (c/ellipse px py p-width p-width)
        
        ;; light-origin
        (c/set-color (:ray-color palette))
        (c/ellipse rx ry light-radius light-radius)

        ;; diagnostic lines
        (c/set-color (:diag-lines palette))
        (c/line rx ry px py)
        (c/ellipse (+ px dx) (+ py dy) 10 10)

        )))

(def palette {:ray-color [238 252 50]
              :planet-obj [100 0 0]
              :diag-lines [0 200 0]})

(let [window-name "light-ray"]
  (c/show-window
    {:canvas (c/canvas 800 600)
     :window-name window-name
     :always-on-top? true
     :state {:ray-pos [0, 0]
             :light-radius 20
             :planet {:color :planet-obj
                      :radius 100}}
     :draw-fn #(draw %1 %2 %3 %4)})

  (defmethod c/mouse-event [window-name :mouse-moved] [e state]
    (let [coord (mouse-pos e)]
      (assoc state :ray-pos coord))))

c/mouse-event-map
