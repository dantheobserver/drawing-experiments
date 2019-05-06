(ns experiments.04-projection
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math]
            [com.rpl.specter :as sp :refer-macros [select transform]]))

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

#_(defn draw-polygon [canvas vp]
    (let [shape '([0 0 0] [0 300 0] [100 50 100] [400 0 44])
          polygon (map #(utils/projected-point % vp) shape)]
      (-> canvas
          (c/set-color [255 0 0])
          (c/filled-with-stroke
           [255 0 0] [0 0 0]
           c/path polygon true)
          #_(c/path polygon true false))))

(defn z-scale
  "Takes in the current entity's `z` coordinate and
  the vanshing point `z-vp` and determines the linear
  scale factor from 0 to `z-vp`"
  [z z-vp]
  (/ (- z-vp z) z-vp))

(defn draw-field [canvas width height [_ _ zp :as vanishing-point]]
  (let [cols 20
        rows 10
        dx (/ width cols)
        dz (/ zp rows)
        base-radius 15]
    (doseq [x (range 0 (+ width dx) dx)
            z (range 0 (+ zp dz) dz)
            :let [radius (-> (z-scale z zp)
                             (* base-radius))
                  [x' y'] (utils/projected-point [x (* height 3/4) z] vanishing-point)]]
      (-> canvas
          (c/ellipse x' y' radius radius)))))

(defn draw-floor-grid
  [canvas width [_ _ vp :as vanishing-point]]
  (let [field-count 3
        margin (/ vp field-count)
        y 400
        base-perimeter [[0 y 0]      ;bottom-left
                        [0 y vp]     ;top left
                        [width y vp] ;top-right
                        [width y 0]  ;bottom-right
                        ]
        shrink-vec [[margin 0 margin]
                    [margin 0 (- margin)]
                    [(- margin) 0 (- margin)]
                    [(- margin) 0 margin]]
        field-point-seq (->> base-perimeter
                             (iterate
                              #(mapv (partial mapv +) % shrink-vec))
                             (take field-count)
                             (projected-points vp))]
    (doseq [field-points field-point-seq]
      (-> canvas
          (c/set-color [0 0 255])
          (c/path field-points true)))))

(defn draw [canvas {:keys [w h] :as window} frame {:keys [vp points] :as state}]
  (-> canvas
      (c/set-background [255 255 255])
      (draw-vanishing-point vp)
      (draw-floor-grid w vp)
      #_(c/set-color [0 0 255])
      #_(c/path (sp/transform [sp/ALL]
                              #(utils/projected-point % vp)
                              [[50 400 0]
                               [50 400 50]
                               [(- w 50) 400 50]
                               [(- w 50) 400 0]])
                true)
      #_(draw-field w h vp))
  state)


(let [w 500
      h 500
      vp [(/ w 2) (/ h 2) 100]
      sections 30
      z-inc (/ 100 sections)
      window-name "point projection"
      window (c/show-window {:canvas (c/canvas w h)
                             :window-name window-name
                             :always-on-top? true
                             :draw-fn #(draw %1 %2 %3 %4)
                             :draw-state (utils/map-keyed vp)
                             :state {:point [0 0]}
                             :fps 30})]

  ;; Update state with mouse position.
  (defmethod c/key-event [window-name :key-pressed] [e state]
    (if (= (c/key-code e) :q)
      (c/close-window window))))


#_(let [shape '([0 0 0] [0 50 30] [100 50 5] [100 0 0])]
    projected (map #(utils/projected-point % [255 255 40]) shape)
    projected)

#_(let [vanishing-point [(/ 500 2) (/ 500 2) 100]]
    (mapv #(projected-points vanishing-point %) '([[0 5 0] [500 5 0] [500 5 100] [0 5 100]]
                                                  [[10 5 10] [490 5 10] [490 5 90] [10 5 90]]
                                                  [[20 5 20] [480 5 20] [480 5 80] [20 5 80]]
                                                  [[30 5 30] [470 5 30] [470 5 70] [30 5 70]]
                                                  [[40 5 40] [460 5
                                                              40] [460 5 60] [40 5 60]]
                                                  [[50 5 50] [450 5 50] [450 5 50] [50 5 50]]
                                                  [[60 5 60] [440 5 60] [440 5 40] [60 5 40]]
                                                  [[70 5 70] [430 5 70] [430 5 30] [70 5 30]]
                                                  [[80 5 80] [420 5 80] [420 5 20] [80 5 20]]
                                                  [[90 5 90] [410 5 90] [410 5 10] [90 5 10]])
          ))
