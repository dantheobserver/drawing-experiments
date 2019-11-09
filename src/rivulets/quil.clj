(ns rivulets.quil
  (:require [quil.core :as q :include-macros true]
            [fastmath.core :as math]
            [fastmath.random :as random]
            [clojure.datafy :as datafy]))

(def width 800)
(def height 800)
(def half-width (/ width 2))
(def half-height (/ height 2))

(def pi (memoize (fn [x] (* math/PI x))))

(defn t [] (q/millis))

(defn move
  [[x y] min-angle max-angle length]
  (let [angle (random/frand min-angle max-angle)]
    [(-> angle math/cos (* length) (+ x))
     (-> angle math/sin - (* length) (+ y))]))

(defn in-bounds?
  [[x y] w h]
  (and (<= 0 x w)
       (<= 0 y h)))

(defn riv-seq
  "Generates a sequence of reviulet points given
  an initial `point` and `angle` of direction.
  boundaries determined by `w` and `h`"
  [point angle w h]
  (let [[x y] point 
        min-angle (- angle (pi 1/4))
        max-angle (+ angle (pi 1/4))
        move-fn #(move % min-angle max-angle 10)]
    (take-while #(in-bounds? % w h)
                (iterate move-fn point))))

;; Quil functions
(defn setup []
  (q/frame-rate 30)
  (q/background 255 255 255)
  (q/set-state!
    :point-seq (riv-seq [half-width 0] (pi 1.5) width height)))

(cons [1 2] '([3 4] [5 6]))
(defn draw []
  (let [[[x1 y1] [x2 y2] & rest] (q/state :point-seq)
        [x y] [x1 y1]]
    (if (and (= x x2) (= y y2))
      (q/set-state! :point-seq (conj [x y] rest)))
    #_(if (and x y)
        (do 
          (q/stroke 0)
          (q/fill (random/irand 255) (random/irand 255) (random/irand 255))
          (q/ellipse x y 15 15)
          (swap! (q/state-atom) assoc :point-seq rest)))))

(q/defsketch rivulet-quil-example
  :title "rivulets in Quil"
  :setup setup,
  :draw draw
  :size [800 600]
  :features [:keep-on-top])

