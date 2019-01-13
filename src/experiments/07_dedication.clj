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

(def env {:orb-mult 3
          :len-mult 16
          :size [300 300]})

(def palette
  [[0 0 0]      [29 43 83]    [126 37 83]   [0 135 81]
   [171 82 54]  [95 87 79]    [194 195 199] [255 241 232]
   [255 0 77]   [255 163 0]   [255 236 39]  [0 228 54]
   [41 173 255] [131 118 156] [255 119 168] [255 204 170]])

(defn p8c->rgb [p8c] (get palette p8c))

(defn next-rad
  [time n]
  (-> (/ time 4)
      (+ (/ n 128))
      math/cos
      (* (env :orb-mult))))

(defn next-pos [x0 y0 time n]
  (let [val (- (r/irand 1) (/ time n)) ;;boundary will be something under 0 or under 1
        x1 (-> (m/cos val) (* (env :len-mult)) (+ x0))
        y1 (-> (m/sin val) (* (env :len-mult)) (+ y0))]
    [x1 y1]))

(defn frame-state
  [time frame-num x-start y-start]
  (let [rad (next-rad time frame-num)
        end (next-pos x-start y-start time frame-num)]
    {:next-origin end
     :line {:start [x-start y-start]
            :end end
            :color (p8c->rgb rad)}
     :circle {:x x-start
              :y y-start
              :radius rad
              :color (p8c->rgb (- rad 4))}}))

(defn clear-screen! [canvas color]
  (-> canvas
      (c/set-background (p8c->rgb color))))

(defn draw-orb!
  [canvas {:keys [x y radius color]}]
  (-> canvas
      (c/set-color color)
      (c/ellipse x y radius radius)))

(defn draw-line!
  [canvas {:keys [color start end]}]
  (-> canvas
      (c/set-color color)
      (c/line start end)))

(defn cur-time [] (System/currentTimeMillis))

(defn draw-loop!
  [frames x0 y0 start-time]
  (fn [canvas window frame loop-state]
    (c/set-background canvas (p8c->rgb 13))
    (dotimes [_ 8]
      (loop [n frames
             [x y] [x0 y0]]
        (if (> n 0)
          (do
            (let [time-diff (- (cur-time) start-time)
                  {:keys [line circle next-origin]} (frame-state time-diff n x y)]
              (-> canvas
                  (draw-orb! circle)
                  (draw-line! line))
              (recur (dec n) next-origin))))))))


(defn start []
  (c/with-canvas->
    (apply c/canvas (env :size))
    (c/show-window "first try" 15 (draw-loop! 32 150 150 (cur-time)))))

(start)
