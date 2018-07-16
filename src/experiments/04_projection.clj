(ns experiments.04-projection
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(defn global-state [window-size]
  {:window-size window-size
   :vanishing-point 100})

(defn object-field [rows cols {:keys [window-size vanishing-point] :as gstate}]
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
              {:x x :y y})}))

(defn draw [canvas window frame _]
  (c/set-background canvas :white)
  (let [{:keys [obj-size objects]} (object-field 4 4 (c/get-state window))
        [w h] obj-size]
    (doseq [{:keys [x y]} objects]
      (-> canvas
          (c/set-color :black)
          (c/ellipse x y w h))))
  nil)

(defn run []
  (let [state (global-state [500 500])
        fps 30
        window-name "perspective"]
    (c/show-window {:canvas (apply c/canvas (:window-size state))
                    :window-name window-name
                    :draw-fn draw
                    :state state
                    :fps fps})))

(run)
