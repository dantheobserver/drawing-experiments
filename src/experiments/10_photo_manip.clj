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
     (let [value (p/get-value p ch x y)]
       (if (and (< 200 x 300)
                (< 150 y 500))
         (rem (+ 10.0 ^double value) 256.0)
         value)))))

(defn draw [canvas window frame {:keys [pixels] :as state}]
  (let [{:keys [filter-idx filters]} (c/get-state window)
        active-filter (get filters filter-idx)
        filtered (p/filter-channels active-filter pixels)]
    (p/set-canvas-pixels! canvas pixels)
    (assoc state :pixels filtered)))

(defn run []
  (let [window-name "pixel manipulation"
        filename "resources/img-a.jpg"
        pixels (p/load-pixels filename)
        window (c/show-window {:canvas (c/canvas (c/width pixels) (c/height pixels) :fast)
                               :window-name window-name
                               :draw-fn (utils/anon-proxy draw)
                               :always-on-top? true
                               :draw-state {:pixels pixels}
                               :state {:filter-idx 0
                                       :filters [cycle-filter shape-filter]}
                               :fps 30})]

    (defmethod c/key-event [window-name :key-pressed] [evt {:keys [filter-idx filters] :as state}]
      (let [total (count filters) 
            val (condp = (c/key-code evt)
                  :up (rem (inc filter-idx) total)
                  :down (mod (dec filter-idx) total))]
        (c/set-state! window (assoc state :filter-idx val))))))
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
