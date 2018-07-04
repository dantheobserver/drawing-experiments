(ns experiments.01-confetti
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def width 800)
(def height 600)

(def canvas-size [width height])
(def center-canvas (into [] (map #(/ ^long % 2) canvas-size)))

(defn rcolor [] (r/irand 255))
(defn rrate [] (r/frand 10 30))

(defn setup [_ _]
  {:droplets (take 1000 (repeatedly (fn []
                                     (let [size (r/irand 10 20)]
                                       {:size [size size]
                                        :color [(rcolor) (rcolor) (rcolor) (r/irand 120 255)]
                                        :rate (rrate)
                                        :pos [(r/frand width) (r/frand height)]}))))})

(defn move-droplet [droplet]
  (let [{:keys [pos ^long rate]} droplet
        [_ ^long y] pos]
    (if (< y ^long height)
      (update-in droplet [:pos 1] #(+ rate ^long %))
      (-> droplet
          (assoc :rate (rrate))
          (assoc-in [:pos 0] (r/frand width)) ;;new x
          (assoc-in [:pos 1] 0)))))

(defn draw [canvas window frame {:keys [droplets] :as state}]
  (c/set-background canvas :black)
  (doseq [droplet droplets]
    (let [{:keys [size color pos]} droplet
          [w h] size
          [x y] pos]
      (-> canvas
          (c/set-color color)
          (c/ellipse x y w h))))
  (update state :droplets #(map move-droplet %)))

(defn run []
  (let [[w h] canvas-size]
    (c/with-canvas [canvas (c/canvas w h)]
      (c/show-window
       {:canvas canvas
        :fps 30
        :draw-fn draw
        :setup setup}))))
