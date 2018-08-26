(ns euclid.core
  (:require [fastmath.core :as math]
            [clojure2d.core :as c]
            [clojure2d.pixels :as p]))

(def pi 3.14159265)

(def circum
  (memoize
   (fn circumf
     ([] (circumf 1))
     ([r] (* 2 pi r)))))

(defn circle [x y r steps]
  (let [incr (/ (circum) steps)]
    (for [i (range 0 steps)
          :let [rad (* incr i)]]
      [(-> rad math/cos (* r) (+ x))
       (-> rad math/sin (* r) (+ y))])))

(defn- flatten-shallow [list] (mapcat identity list))

(defn draw
  [canvas window frame state]
  (doseq [[x y] (flatten-shallow state)]
    (-> canvas
        (c/set-color :white)
        (c/rect x y 1 1))))

(let [state {:window-name "eq-triangle"
             :window-size [500 500]
             :fps 30}
      [mx my] (into [] (map #(/ % 2)) (:window-size state))
      steps 360
      draw-state (list  (circle (- mx 50) my 100 steps)
                        (circle (+ mx 50) my 100 steps))]
  (c/show-window {:canvas (apply c/canvas (:window-size state))
                  :window-name (:window-name state)
                  :draw-fn draw
                  :state state
                  :draw-state draw-state
                  :fps (:fps state)}))
;; ex
;; (circle 0 0 2 8) ; Should give a circle with 8 points
