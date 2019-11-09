(ns experiments.14-amorphous.quil
  (:require [quil.core :as q]
            [experiments.14-amorphous.core
             :refer [->Amorph gen-points section-triangles]
             :as am]))

(def pallate
  {:skeleton [0 0 0]
   :section [80 250 100]})

(def amorph (->Amorph [400 300]
                      (gen-points 100 10 [400 300]
                                  (am/random-radius-calc-fn 70 200))))

(defn setup []
  (println "testing")
  (q/frame-rate 12)
  (q/background 255 255 255)
  (q/set-state! :amorph amorph))

(defn pallate-fn [fn pallate-key] (apply fn (get pallate pallate-key)))
(defn pallate-stroke [pallate-key] (pallate-fn q/stroke pallate-key))
(defn pallate-fill [pallate-key] (pallate-fn q/fill pallate-key))

(defn draw-sections
  [points center]
  (doseq [triangle (section-triangles points center)]
    (apply q/triangle (flatten triangle))))

(defn draw []
  (let [{:keys [center points]} (q/state :amorph)]
    ;;draw sections
    (pallate-fill :section)
    (pallate-stroke :skeleton)
    (draw-sections points center)))

(q/defsketch amorph-quil-example
  :title "amorph in Quil"
  :setup setup
  :draw draw
  :size [800 600]
  :features [:keep-on-top])

#_(-> amorph
      :points
      (section-triangles [0 0])
      first
      flatten)
