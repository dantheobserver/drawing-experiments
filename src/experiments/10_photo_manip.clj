(ns experiments.10-photo-manip
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math]
            [fastmath.fields :as f]))

(def cycle-filter
  (p/filter-channel #(rem (+ 10.0 ^double %) 256.0)))

(def shape-filter
  (p/filter-channel-xy
   (fn [ch p x y]
     (if (and (< 200 x 300)
              (< 150 y 500))
       (rem (+ 10.0 ^double ch) 256.0)
       ch))))

(defn draw [canvas window frame {:keys [pixels] :as state}]
  (let [filtered (p/filter-channels #_cycle-filter shape-filter pixels)]
    (p/set-canvas-pixels! canvas pixels)
    (assoc state :pixels filtered)))

(defn run []
  (let [window-name "pixel manipulation"
        filename "./resources/img-a.jpg"
        pixels (p/load-pixels filename)
        window (c/show-window {:canvas (c/canvas (c/width pixels) (c/height pixels))
                               :window-name window-name
                               :draw-fn (utils/anon-proxy draw)
                               :always-on-top? true
                               ;; :draw-fn draw
                               :draw-state {:pixels pixels}
                               :fps 30})]


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
