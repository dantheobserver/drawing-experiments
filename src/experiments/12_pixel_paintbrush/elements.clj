(ns experiments.12-pixel-paintbrush.elements
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [com.rpl.specter :refer [transform ALL]]))

(set! *unchecked-math* :warn-on-boxed)

(defprotocol DrawnElement
  (next-frame [this t])
  (draw-frame [this canvas color]))

(defrecord PulsePixel [x y intensity]
  DrawnElement
  (next-frame [this t]
    (let [int-factor (/ 255 2)
          t-factor 7
          intensity (-> t (/ t-factor) m/sin (* ^int int-factor) (+ ^double int-factor))]
      (assoc this :intensity intensity)))
  (draw-frame [this canvas color]
    (-> canvas
        (c/set-color color (conj color intensity))
        (c/ellipse x y 10 10))))

(defn update-points!
  "Updates a set of points by by calling next-frame"
  [window frame]
  (let [state (c/get-state window)
        next-state (transform [:points ALL] #(next-frame % frame) state)]
    (c/set-state! window next-state)))

#_(transform [:points ALL] inc {:pocints [1 2 3]})
