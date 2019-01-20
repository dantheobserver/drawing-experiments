(ns experiments.09-time-experiments
  (:require [fastmath.core :as math]
            [fastmath.random :as r]
            [clojure2d.core :as c]
            [clojure2d.color :as clr]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [common.utils :as utils]
            [clojure2d.color :as color]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def w 800)

(def h 800)

(def window-name "time-widget")

(defn cur-time
  [^long fps ^long frame]
  (* (/ 1 fps) frame))
(number? 1/60)
;;2pi/s
;;
(defn draw-dot
  [canvas {:keys [pos] :as state} time]
  (let [[x y] pos
        wave-length (/ ^long h 2)
        secs-in-rad (* time math/TWO_PI) ;every sec is 2pi
        y' (+ y (* wave-length (math/sin secs-in-rad)))]
    (-> canvas
        (c/set-color (color/color :red))
        (c/ellipse x y' 20 20))))

(defn draw [canvas {:keys [fps]} frame state]
  (-> canvas
      (c/set-background (color/color :white))
      (draw-dot state (cur-time fps frame)))
  state)

(def window
  (c/show-window
   {:canvas (c/canvas w h)
    :fps 60
    :window-name window-name
    :draw-fn (utils/anon-proxy draw)
    :draw-state {:pos (mapv #(/ ^long % 2) [w h])}}))

(defmethod c/key-typed [window-name \q] [event state]
  (c/close-window window))

(.setAlwaysOnTop (:frame window) true)
