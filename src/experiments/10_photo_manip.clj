(ns experiments.10-photo-manip
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math]))

(defn cycle-col [val]
  #_(r/irand 0 256)
  (rem (+ 10 val) 256))

(defn draw [canvas window frame {:keys [pixels] :as state}]
  (let [filtered (p/filter-colors-xy (fn [p x y]
                                       (let [[r g b a] (p/get-color p x y)
                                             avg (/ (+ r g b) 3)]
                                         ;; (if (< 20 avg))
                                         [(cycle-col r) (cycle-col g) (cycle-col b) a]
                                         ))
                                     pixels)]
    (p/set-canvas-pixels! canvas pixels)
    (assoc state :pixels filtered)))

(defn run []
  (let [window-name "pixel manipulation"
        filename "./resources/my_eye_and_lsd_by_paranoidplatyplus88-d720rb0.jpg"
        pixels (p/load-pixels "./resources/img-a.jpg")
        window (c/show-window {:canvas (c/canvas (c/width pixels) (c/height pixels))
                               :window-name window-name
                               :draw-fn (utils/anon-proxy draw)
                               ;; :draw-fn draw
                               :draw-state {:pixels pixels}
                               :fps 30})]

    (.setAlwaysOnTop (:frame window) true)

    ;; Update state with mouse position.
    #_(defmethod c/mouse-event [window-name :mouse-moved] [e state]
        (let [mouse-pos ((juxt c/mouse-x c/mouse-y) e)]
          (c/set-state! window (assoc state :point mouse-pos))))))
(run)

(comment
  ;; Getting an image loaded into pixels
  (let [pixels (p/load-pixels "./resources/img-a.jpg")
        filtered (p/filter-colors-xy (fn [p x y]
                                       [0 0 0 255])
                                     pixels)]
    #_pixels
    filtered
    (c/width pixels)
    #_(p/set-color pixels 0 0 [0 0 0])
    (p/get-color filtered 0 0)))
