(ns experiments.01-confetti
  (:require [clojure2d.core :as c]
            [clojure2d.color :as color]
            [fastmath.core :as m]
            [fastmath.random :as r]))

(def width 800)
(def height 600)
(def canvas-size [width height])
(def center-canvas (into [] (map #(/ % 2) canvas-size)))


(defn rcolor [] (r/irand 255))

(defn setup [_ _]
  {:droplets (take 1000 (repeatedly (fn []
                                     (let [size (r/irand 10 20)]
                                       {:size [size size]
                                        :color [(rcolor) (rcolor) (rcolor) (r/irand 120 255)]
                                        :rate (r/frand 10 30)
                                        :pos [(r/frand width) 0]}))))})

(defn move-droplet [droplet]
  (let [{:keys [pos rate]} droplet 
        [_ y] pos]
    (if (< y height)
      (update-in droplet [:pos 1] #(+ rate %))
      (-> droplet
          (assoc :rate (r/frand 10 30))
          (assoc-in [:pos 0] (r/frand width)) ;;new x
          (assoc-in [:pos 1] 0)))))

(defn draw [canvas window frame {:keys [droplets] :as state}]
  (c/set-background canvas :black)
  (doall
   (for [droplet droplets]
    (let [{:keys [size color pos]} droplet
           [w h] size
           [x y] pos]
       (-> canvas
           (c/set-color color)
           (c/ellipse x y w h)))))
  (update state :droplets #(map move-droplet %)))

(defn run []
  (let [[w h] canvas-size]
    (c/with-canvas [canvas (c/canvas w h)]
      (c/show-window
       {:canvas canvas
        :fps 30
        :draw-fn draw
        :setup setup}))))

(run)
