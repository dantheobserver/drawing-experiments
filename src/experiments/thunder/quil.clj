(ns experiments.thunder.quil
  (:require [quil.core :as q]
            [fastmath.core :as m]))

;;Thunder effect
;;Actors:
;;  Creature: quadrapeal monster that has cloudy surface
;;  You
;;Play:
;;  You are visible, your background and creature are not


;;;01234567890123456
;;0....o.......o....
;;1   /       /
;;2  o       o
;;3   \       \
;;4    o       o

(def creature {:position [0 0]
               :head [12 0] ;; head generated separately
               :sections '([0 0 4 0] ;;tail
                           [4 0 12 0] ;;torso
                           [4 0 2 2] ;; hind leg
                           [2 2 4 4] ;; hind paw
                           [12 0 10 2] ;; front leg
                           [10 2 12 4] ;; front paw
                           )})

(defn scale [sections mag]
  (map #(mapv (partial * mag) %) sections))

(defn translate [sections [x y]]
  (map #(mapv + [x y x y] %) sections))

(defn line-points [sections x1 y1 x2 y2]
  (let [slope (/ (- y2 y1)
                 (- x2 x1))
        y-int (- y1 (* slope x1))
        x-inc (/ (- x2 x1) sections)
        iter-fn (fn [[x _]]
                  (let [nx (+ x x-inc)
                        ny (+ (* slope nx) y-int)]
                    [nx ny]))]
    (take (inc sections) (iterate iter-fn [x1 y1]))))

(defn draw-section [[x1 y1 x2 y2]]
  (q/line x1 y1 x2 y2)
  (doseq [[x y] (line-points 3 x1 y1 x2 y2)]
    (q/line x1 y1 x2 y2)
    (q/ellipse x y 10 10)))

(defn setup []
  (q/frame-rate 12)
  (q/background 255 255 255)
  (q/set-state! :creature creature))

(defn draw []
  (q/background 255 255 255)
  (doseq [section (-> creature
                      :sections
                      (scale 45)
                      (translate [35 50]) )]
    (draw-section section)))
(-> creature
    :sections
    (scale 20)
    (translate [10 10]))
(q/defsketch creature-quil-example
  :title "Quadropedal creature sketch"
  :setup setup
  :draw draw
  :size [800 600]
  :features [:keep-on-top])




