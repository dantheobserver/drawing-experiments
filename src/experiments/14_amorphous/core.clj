(ns experiments.14-amorphous.core
  (:require [clojure2d.core :as c]
            [fastmath.random :as random]
            [fastmath.core :refer [TWO_PI sin cos]]))

(defrecord Amorph [center points])

(defn default-calc-fn
  "normal consistent distribution, no variations"
  [[x y] angle radius]
  [(+ x (* (cos angle) radius))
   (+ y (* (sin angle) radius))])

;; TODO: creates an odd seam on the last rotation due to randomness,
;; ttry to match opening and closing points
(defn random-radius-calc-fn
  [min-radius max-radius]
  (fn [[x y] angle _]
    (let [radius (random/frand min-radius max-radius)]
      [(+ x (* (cos angle) radius))
       (+ y (* (sin angle) radius))])))

(defn gen-points
  ([radius sections origin]
   (gen-points radius sections origin default-calc-fn))
  ([radius sections origin calc-fn]
   (let [point-count (inc sections)
         step (/ TWO_PI point-count)]
     (loop [[angle & angles] (range 0 TWO_PI step)
            first-calc nil
            res []]
       (let [current (calc-fn origin angle radius)]
         (cond
           (nil? angle) res
           (nil? angles) (conj res first-calc)
           :else (recur angles
                        (or first-calc current)
                        (conj res current))))))))


(defn section-triangles
  [[p & points] center]
  (if (nil? points) []
      (let [looped-points (concat [p] points [p])]
        (loop [[p q & points] looped-points
               triangles []]
          (cond
            (and p q) (recur (cons q points)
                             (conj triangles [p q center]))
            :else triangles)))))


#_(gen-points 30 4 [0 0] (random-radius-calc-fn 30 60))
#_(gen-points 100 10 [400 300])
#_(circle-points 10 4)

#_(defn draw-amorph
    [canvas {:keys [x y points center]} color]
    ;;draw amorph
    (-> canvas
        (c/set-color color)
        (c/path points))
    ;;draw outline
    (let [[cx cy] center]
      (doseq [[px py] points]
        (-> canvas
            (c/set-color :black)
            (c/line cx cy px py)))))

#_(defn draw [canvas window frame state]
    (-> canvas
        (draw-amorph state [200 100 30]))
    state)

#_(def window-name "amorphs")
#_(def dimension [800 600])
#_(def canvas (apply c/canvas dimension))
#_(def window (c/show-window {:canvas canvas
                              :window-name window-name
                              :draw-fn #(draw %1 %2 %3 %4)
                              :draw-state (->Amorph 400 300 (gen-points 130 10 [400 300]) [400 300])
                              :always-on-top? true}))

