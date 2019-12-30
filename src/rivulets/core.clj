(ns rivulets.core
  (:require [com.rpl.specter :refer [transform ALL]]
            [fastmath.core :as math]
            [fastmath.random :as random]))

(def pi (memoize (fn [x] (* math/PI x))))

(def dist (memoize math/dist))

#_(defn t
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

(defprotocol Drawable
  (next-frame [this frame]))

(defrecord Rivulet [point-seq draw-frame x y]
  Drawable
  (next-frame [this frame] 
    (when point-seq
      (let [[[x1 y1] [x2 y2]] point-seq
            time (-> frame
                     (- draw-frame)
                     (* 0.3))
            dx (* time (- x2 x1)) 
            dy (* time (- y2 y1))
            x' (+ x1 dx)
            y' (+ y1 dy)
            total-dist (math/dist x1 y1 x2 y2)
            drawn-dist (math/dist x1 y1 x' y')]
        (if (>= drawn-dist total-dist)
          (assoc this
                 :point-seq (next point-seq)
                 :draw-frame frame
                 :x x1 :y y1)
          (assoc this :x x' :y y'))))))

(defn make-rivulet [point angle w h options]
  (let [points (riv-seq point angle w h options)
        [x y] (first points)]
    (->Rivulet points 0 x y)))

(defrecord CoilCircle [origin radius line-points]
  Drawable
  (next-frame [this frame]))

(defn circle-point [[x y] angle radius]
  [(-> angle math/cos (* radius) (+ x))
   (-> angle math/sin (* radius) (->> (- y)))])

(defn make-coil-circle [origin radius resolution]
  (let [[x y] origin
        radius radius
        res resolution
        angle-seq (take res (iterate #(+ % (/ (pi 2) res)) 0))
        line-angles (partition 2 1 (list (pi 2)) angle-seq)
        line-segments (transform [ALL ALL]
                                 #(circle-point origin % radius)                                 
                                 line-angles)]
    (->CoilCircle origin radius line-segments)))

(defrecord ResponseCircle [origin radius trail-length]
  Drawable
  (next-frame [this _]))

(comment
  (defmacro when-let [bindings & body]
    (let [b-count (count bindings)]
      (if (and (> 0 b-count)
               (rem b-count))
        )))
  (some->  '(1)
           next
           empty?)
  (rem 1 2))
