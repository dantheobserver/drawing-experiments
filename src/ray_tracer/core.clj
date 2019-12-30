(ns ray-tracer.core
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [ray-tracer.vector3 :as vec3]))

;; Reading
(defn p3-write!
  [filename body w h]
  (let [header (str (line "P3")
                    (line w " " h) "255")]
    (spit filename header)
    (spit filename body :append true)))

(defn create-ray [vec3-origin vec3-direction]
  {:origin vec3-origin
   :direction vec3-direction})

(def ray-origin :origin)
(def ray-direction :direction)

(defn point-at-param
  [{:keys [origin direction]} t]
  (vec3/plus origin (vec3/scalar-multiply t direction)))

(defn color [{:keys [direction]}]
  (let [y (get direction 1)
        t (* 0.5 (+ 1 y))
        white [1 1 1]
        blue [0.5 0.7 1]]
    (vec3/plus (vec3/scalar-multiply (- 1 t) white)
               (vec3/scalar-multiply t blue))))

(let [w 200
      h 100
      lower-left-corner [-2 -1 -1]
      origin [0 0 0] ;;camera origin
      horizontal [4 0 0]
      vertical [0 2 0]  
      content (for y (range (dec h) -1 -1) ;; range from width to 0
                   x (range 0 w)           ;; range from 
                   :let [;; fraction of width of current x coordinate
                         u (/ x w)
                         ;; fraction of width of current y coordinate
                         v (/ y h)
                         ;; Vector on plane coordinate
                         vector-on-plane (vec3/plus (vec3/scalar-multiply u horizontal)
                                                    (vec3/scalar-multiply v vertical))
                         ;; offset 
                         direction (vec3/plus lower-left-corner vector-on-plane)
                         ray (create-ray origin direction)
                         pixel-color (color ray)
                         [color (->> [0 1 2]
                                     (mapv pixel-color)
                                     (mapv #(int (* 255.99 %))))]])])

;;Comment block
(comment
  'comment-block

  ;; P(t) = ray-origin + (t * ray-direction) 
  ;; Gives a point along the ray of ray-origin and ray-direction
  (let [ray-origin [0 0]
        ray-direction [1 1]
        t-vals (range 0 1.1 0.1)]
    (for [t t-vals]
      (mapv + ray-origin
            (mapv #(* t %) ray-direction))))





  )
