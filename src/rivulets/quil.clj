(ns rivulets.quil
  (:require [quil.core :as q :include-macros true]
            [fastmath.core :as math]
            [fastmath.random :as random]
            [clojure.datafy :as datafy]
            [quil.middleware :as m]))

(def width 800)
(def height 800)
(def half-width (/ width 2))
(def half-height (/ height 2))

(def pi (memoize (fn [x] (* math/PI x))))
(def dist (memoize math/dist))

(defn t
  "Gives the quil time in miliseconds.
  can be provide a `factor` to multiply by."
  ([] (t 1))
  ([factor] (* (q/millis) factor)))

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
  [point angle w h options]
  (let [{:keys [length sweep]} (merge {:length 10
                                       :sweep (pi 1/4)}
                                      options)
        [x y] point 
        min-angle (- angle sweep)
        max-angle (+ angle sweep)
        move-fn #(move % min-angle max-angle length)]
    (take-while #(in-bounds? % w h)
                (iterate move-fn point))))
(merge {:a 'default} nil)

(defrecord rivulet [point-seq draw-frame x y])
;; Quil functions
(defn setup []
  (q/frame-rate 30)
  (q/background 255 255 255)
  (let [points (riv-seq [half-width 0] (pi 1.5) width height {:sweep (pi 1/8) :length 20})
        [x y] (first points)]
    (q/set-state!
      :rivulets ()
      :point-seq points
      :draw-frame 0
      :x x :y y)))

#_(cons [1 2] '([3 4] [5 6]))
(defn draw [state]
  (if (nil? (:point-seq state))
    (q/no-loop))
  (let [{:keys [point-seq x y]} state
        [x1 y1] (first point-seq)]
    (q/fill 255 0 0)
    (q/line x1 y1 x y)))

(defn update-state [state]
  ;; perhaps create sequence of sub-points to render in draw
  ;; easier to wrap head around.
  (let [point-seq (:point-seq state)
        [[x1 y1] [x2 y2]] point-seq
        time (-> (q/frame-count)
                 (- (:draw-frame state))
                 (* 0.3))
        dx (* time (- x2 x1)) 
        dy (* time (- y2 y1))
        x (+ x1 dx)
        y (+ y1 dy)
        total-dist (math/dist x1 y1 x2 y2)
        drawn-dist (math/dist x1 y1 x y)
        ]
    (if (>= drawn-dist total-dist)
      (do
        (assoc state
               :point-seq (next point-seq)
               :draw-frame (q/frame-count)
               :x x1 :y y1))
      (assoc state :x x :y y))
    ))

(q/defsketch rivulet-quil-example
  :title "rivulets in Quil"
  :setup setup,
  :draw draw
  :update update-state
  :size [800 600]
  :features [:keep-on-top]
  :middleware [m/fun-mode])

(comment
  ;; Create a line drawing effect 
  (let [[x1 y1] [(/ width 2) 0]
        [x2 y2] [(/ width 2) height]
        dx (- x2 x1)
        dy (- y2 y1)
        time (t 0.0001)]
    (q/fill 255 0 0)
    (q/line x1
            y1
            (+ x1 (* time dx))
            (+ y1 (* time dy)))))
