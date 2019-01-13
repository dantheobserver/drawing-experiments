(ns crazy-lines.core
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.core :as math]))

;; Model
(set! *unchecked-math* :warn-on-boxed)

(defn pi
  ([] 3.14159)
  ([x] (* ^double (pi) ^double x)))

(defrecord Gear [^double radius
                 ^doubles position ;;
                 ^double line-angle ;;revolution in rads
                 ^double rotation-rate ;;turn rate, rads.
                 ])
(defn make-gear
  [radius position line-angle rotation-rate]
  (->Gear radius position line-angle rotation-rate))

(defn line-edge-coord
  "The coordinate where the line meets the surface of the gear"
  [{:keys [^double radius ^doubles position ^double line-angle]}]
  (let [[gx gy] position]
    [(-> line-angle math/cos (* ^double radius) (+ ^double gx))
     (-> line-angle math/sin (* ^double radius) (+ ^double gy))]))

(defn rotate
  [{:keys [^double line-angle ^double rotation-rate] :as gear}]
  (->> line-angle
      (+ rotation-rate)
      (assoc gear :line-angle)))

(comment
  (do
    (let [test-gear (->Gear 30.0 [0 0] 0 (pi 2))]
      (->> test-gear
           rotate
           line-edge-coord
           (map math/round))))
  )
