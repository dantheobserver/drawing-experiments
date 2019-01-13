(ns experiments.04-projection
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils :use [pi-memoized :as pi]]
            [fastmath.core :as math]))

(def w 500)
(def h 500)

(defn draw-vanishing-point [canvas [xv yv]]
  (-> canvas
      (c/set-color [255 0 0])
      (c/ellipse xv yv 5 5)))

(defn- projected-point
  [[x y z :as point]
   [xv yv zv :as vanishing-point]]
  (let [dist (m/dist x y xv yv)
        dx (- xv x)
        dy (- yv y)
        pt-dist (* z (/ dist zv))
        ratio (/ pt-dist dist)
        offsets (mapv (partial * ratio) [dx dy])]
    (mapv + [x y] offsets)))

(defn draw-points
  [canvas z-points [x y] vp]
  (doseq [z z-points]
    (let [ [x' y'] (projected-point [x y z] vp)]
      (-> canvas
          (c/set-color [0 0 255])
          (c/ellipse x' y' 4 4)))))

(defn draw [canvas window frame {:keys [vp points] :as state}]
  (-> canvas
      (c/set-background [255 255 255])
      (draw-vanishing-point vp)
      (draw-points points (:point (c/get-state window)) vp))
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
                               :draw-fn (utils/proxy-fn draw)
                               :draw-state (utils/map-keyed vp points)
                               :state {:point [0 0]}
                               :fps 30})]

    (.setAlwaysOnTop (:frame window) true)

    (defmethod c/mouse-event [window-name :mouse-moved] [e state]
      (let [mouse-pos ((juxt c/mouse-x c/mouse-y) e)]
        (c/set-state! window (assoc state :point mouse-pos))
        )))
  )


(run)
