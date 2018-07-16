(ns experiments.04-projection
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(defn global-state [window-size]
  {:window-size window-size
   :vanishing-point 100})

(defn object-field [rows cols z {:keys [window-size vanishing-point] :as gstate}]
  (let [[w h] window-size
        obj-pct 0.75                 ;; percentage of empty space surrouding object
        row-height (/ h rows)
        col-width (/ w cols)
        height-offset (/ row-height 2)     ;;coord-placement
        width-offset (/ col-width 2)]
    {:obj-size [(* col-width obj-pct) (* row-height obj-pct)]
     :objects (for [r (range rows)
                  c (range cols)
                  :let [x (-> c (* col-width) (+ width-offset))
                        y (-> r (* row-height) (+ height-offset))]]
              {:x x :y y :z z})}))

(defn project-obj
  "Project object with offset size and position based
  on z-index"
  [canvas {:keys [x y z]} [w h] max-z]
  (let [z-pct (- 1 (/ z max-z))]
    (c/ellipse canvas x y (* w z-pct) (* h z-pct))))

(defn draw [canvas window frame {:keys [obj-size objects] :as state}]
  (c/set-background canvas :white)
  (let [[w h] obj-size
        t (/ frame (:fps window))
        {:keys [vanishing-point]} (c/get-state window)]
    (doseq [obj objects]
      (-> canvas
          (c/set-color :black)
          (project-obj obj obj-size vanishing-point))))
  state)

(defn run []
  (let [state (global-state [500 500])
        fps 30
        window-name "perspective"]
    (c/show-window {:canvas (apply c/canvas (:window-size state))
                    :window-name window-name
                    :draw-fn draw
                    :state state
                    :draw-state (object-field 4 4 0 state)
                    :fps fps})))

(run)
