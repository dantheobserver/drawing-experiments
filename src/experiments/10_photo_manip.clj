(ns experiments.10-photo-manip
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math]))

(defn draw [canvas window frame {:keys [pixels] :as state}]
  (let [filtered (p/filter-colors-xy (fn [p x y]
                                       (->> (p/get-color p x y)
                                            (mapv #(rem (+ 10 %) 256))))
                                     pixels)]
    (p/set-canvas-pixels! canvas pixels)
    (assoc state :pixels filtered)))

(defn run []
  (let [window-name "pixel manipulation"
        filename "./resources/my_eye_and_lsd_by_paranoidplatyplus88-d720rb0.jpg"
        pixels (p/load-pixels "./resources/my_eye_and_lsd_by_paranoidplatyplus88-d720rb0.jpg")
        window (c/show-window {:canvas (c/canvas (c/width pixels) (c/height pixels))
                               :window-name window-name
                               :draw-fn (utils/anon-proxy draw)
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
