(ns experiments.07-dedication
  (:require [fastmath.core :as math]
            [fastmath.random :as r]
            [clojure2d.core :as c]
            [clojure2d.color :as clr]
            [fastmath.core :as m]))

;; r=rnd::_::cls(13)srand()
;;;; alias rand, set goto point, clear screen to 13, seed random
;; function a(n,x,y)
;;;; delare function; n = number of frames
;; k=cos(t()/4+n/128)*3
;;;;calculate k
;; d=x+cos(r(1)-t()/n)*8
;; e=y+sin(r(1)-t()/n)*8
;; draw line from [x y] to [d, e] of color k
;; line(x,y,d,e,k)
;; create circle at origin [x y] of radius k and color k-4
;; circfill(x,y,k,k-4)
;; recur, decreasing n
;; if(n>0)a(n-1,d,e)
;; end

;; for i=0,8 do
;; a(32,64,64)
;; end
;; flip()goto _

(def palette
  [[0, 0, 0] [29, 43, 83] [126, 37, 83] [0, 135, 81]
   [171, 82, 54] [95, 87, 79] [194, 195, 199] [255, 241, 232]
   [255, 0, 77] [255, 163, 0] [255, 236, 39] [0, 228, 54]
   [41, 173, 255] [131, 118, 156] [255, 119, 168] [255, 204, 170]])

(defn p8c->rgb [p8c] (get palette p8c))

(defn pos-calc [x y time n]
  (let [val (- (r/irand 1) (/ time n)) ;;boundary will be something under 0 or under 1
        x' (-> x
               (+ (m/cos val))
               (* 8))
        y' (-> y
               (+ (m/sin val))
               (* 8))]
    [x y]))

(defn frame-state
  [time frame-num x-start y-start]
  (let [rad (-> time
                (/ (+ 4 frame-num) 128)
                math/cos
                (* 8))
        [x-end  y-end] (pos-calc x-start y-start time frame-num)]
    {:line {:start [x-start y-start]
            :end [x-end y-end]
            :color (p8c->rgb rad)}
     :circle {:x x-start
              :y y-start
              :radius rad
              :color (p8c->rgb (- rad 4))}}))


(defn clear-screen! [canvas color]
  (-> canvas
      (c/set-background (p8c->rgb color))))

(defn draw-frame!
  [canvas {:keys [line circle]}]
  (-> canvas
      (c/set-color (:color line))
      (c/line (line :start) (line :end))
      (c/set-color (:color circle))
      (c/ellipse (circle :x)
                 (circle :y)
                 (circle :radius)
                 (circle :radius))))

(defn cur-time [] (System/currentTimeMillis))

(defn draw-loop!
  [frames x y]
  (fn [canvas window frame loop-state]
    (c/set-background canvas (p8c->rgb 13))
    (loop [frames frames
           start-time (cur-time)]
      (if (> frames 0)
        (do
          (let [time-diff (- (cur-time) start-time)
                next-frame (frame-state time-diff frames x y)]
            (-> canvas
                (draw-frame! next-frame))
            (recur (dec frames) start-time)))))))


(c/with-canvas->
  (c/canvas 300 300)
  (c/show-window "first try" (draw-loop! 32 64 64)))
