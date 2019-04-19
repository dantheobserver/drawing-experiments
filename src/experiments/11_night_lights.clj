(ns experiments.11-night-lights
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :refer [pi]]
            [fastmath.core :as math]))

(set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)
(set! *unchecked-math* true)

(def move-down +)
(def move-up -)

(defrecord Beam [x y0 y1])

(defprotocol BeamPathElement
  (move-along-beam [_])
  (draw-element [_ canvas]))

(defrecord Glimmer [^double x ^double y ^int brightness direction ^Beam beam]
  BeamPathElement
  (move-along-beam [_]
    (let [{beam-x :x beam-y0 :y0 beam-y1 :y1} beam
          x-buffer 1.0
          #_next-x  #_(+ beam-x
                         (r/frand
                          (- x x-buffer)
                          (+ x x-buffer)))
          next-y (move-down y (r/frand 2))] ;;magic random
      (->Glimmer 
       beam-x
       ;; return for bobble
       #_(if (< (- x x-buffer) next-x (+ x x-buffer))
           next-x
           (- next-x))
       (if (< beam-y0 ^float next-y) beam-y1 next-y)
       brightness #_next-brightness
       direction
       beam)))
  (draw-element [_ canvas]
    (let [size 2]
      (-> canvas
          (c/set-color [(r/irand 30 255)
                        (r/irand 30 255)
                        (r/irand 30 255)] #_[255 255 255])
          (c/line x y x (+ y size))
          #_(c/line x y (+ x 10) (- y size)) ;fuzzy
          #_(c/ellipse x y 1 1)))))

;; state generation

(defn gen-beams
  "Generate beams along a set width with set heights"
  [^long w ^long h beam-margin]
  (for [x (range 0 w beam-margin)]
    (let [x-rad (* x (/ (pi 4) w))]
      (->Beam x h (- h (-> x-rad math/sin math/abs (* h)))))))

(defn
  gen-glimmers
  ([beams] (gen-glimmers beams 1))
  ([beams glimmer-gen]
   (flatten 
    (map (fn [{:keys [x y0 y1] :as beam}]
           (map #(->Glimmer x % 255 1 beam)
                (range y1 y0 20)))
         beams))))

(defn get-draw-state [w h]
  (let [beam-margin 10 
        beams (gen-beams w h 3)
        glimmers (gen-glimmers beams)]
    #_(println glimmers)
    {:beams beams :glimmers glimmers}))

;;draw
(defn draw-beam
  [canvas {:keys [x y0 y1]} color]
  (-> canvas
      (c/set-color color)
      (c/line x y0 x y1)))

(defn draw [canvas window frame state]
  (c/set-background canvas [0 0 0])
  (let [glimmers (:glimmers state)] 
    (doseq [glimmer glimmers]
      (-> canvas
          (->> (draw-element glimmer)))))
  #_state
  (-> state
      (update :glimmers (partial map move-along-beam))))

(let [w 800
      h 550
      name "night-lights"]
  (c/show-window {:window-name name 
                  :canvas (c/canvas w h)
                  :always-on-top? true
                  :state {:w w :h h}
                  :fps 60
                  :draw-state (get-draw-state w h) ;(draw-state w h)
                  :draw-fn #(draw %1 %2 %3 %4)}))
