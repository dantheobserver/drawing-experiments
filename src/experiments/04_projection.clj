(ns experiments.04-projection
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math]))

(def w 500)
(def h 500)

(defn draw-vanishing-point [canvas [xv yv]]
  (-> canvas
      (c/set-color [255 0 0])
      (c/ellipse xv yv 5 5)))

(defn draw-points
  [canvas points vp]
  (doseq [point points]
    (let [[x' y'] (utils/projected-point point vp)]
      (-> canvas
          (c/set-color [0 0 255])
          (c/ellipse x' y' 4 4))))
  canvas)

(defn draw-polygon [canvas vp]
  (let [shape '([0 0 0] [0 300 0] [100 50 100] [400 0 44])
        polygon (map #(utils/projected-point % vp) shape)]
   (-> canvas
        (c/set-color [255 0 0])
        (c/filled-with-stroke
         [255 0 0] [0 0 0]
         c/path polygon true)
        #_(c/path polygon true false))))

(defn draw [canvas window frame {:keys [vp points] :as state}]

  (-> canvas
      (c/set-background [255 255 255])
      (draw-vanishing-point vp)
      (draw-polygon vp)
      ;; (draw-points points vp)
      #_(draw-points points vp)
      #_(draw-points points (:point (c/get-state window)) vp))
  state)

(defn run []
  (let [vp [(/ w 2) (/ h 2) 100]
        sections 30
        z-inc (/ 100 sections)
        points (->> (take sections (iterate #(+ z-inc %) 0))
                    (map m/round))
        window-name "point projection"
        window (c/show-window {:canvas (c/canvas w h)
                               :window-name window-name
                               :draw-fn (utils/anon-proxy draw)
                               :draw-state (utils/map-keyed vp points)
                               :state {:point [0 0]}
                               :fps 30})]

    (.setAlwaysOnTop (:frame window) true)

    ;; Update state with mouse position.
    #_(defmethod c/mouse-event [window-name :mouse-moved] [e state]
        (let [mouse-pos ((juxt c/mouse-x c/mouse-y) e)]
          (c/set-state! window (assoc state :point mouse-pos))))))

(run)

#_(let [shape '([0 0 0] [0 50 30] [100 50 5] [100 0 0])
        projected (map #(utils/projected-point % [255 255 40]) shape)]
    projected)
