(ns experiments.letters.sketch
  (:require [quil.core :as q]))

(def letters {:a []})

(defn setup []
  (println "letters")
  (q/frame-rate 30)
  (q/background 255 255 255))

(defn draw []
  (q/background 255 255 255))

(q/defsketch drawing-letters
  :title "Drawing letters"
  :setup setup
  :draw draw
  :size [800 600]
  :features [:keep-on-top])
