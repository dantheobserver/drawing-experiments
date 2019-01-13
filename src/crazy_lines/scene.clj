(ns crazy-lines.scene
  (:require [crazy-lines.core :as core]
            [fastmath.core :as math]
            [fastmath.random :as r]
            [clojure2d.core :as c]
            [clojure2d.pixels :as p]
            [fastmath.fields :as f]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def scene-size [800 800])
(def mid-position (mapv #(/ ^double % 2) scene-size))
(def options {:speed-limit 0.1099
              :max-marks 500})

;; Data generation functions
(defn random-gear
  ([pos] (random-gear pos #(r/drand 10 88)))
  ([pos size-fn]
   (core/make-gear (size-fn)
                   pos
                   (r/drand (core/pi 2))
                   (r/drand (:speed-limit options)))))

(defn attached-gear
  "Generates a new gear attached to the end of the line of the previous gear"
  [gear]
  (let [next-position (core/line-edge-coord gear)
        {:keys [^double radius]} gear]
    (random-gear next-position #(r/drand (/ radius 2) radius))))

(defn gen-gears
  "Generate a sequence of random gears starting
  at the origin and each sequencial gear originating
  at the terminus of the line of the next"
  [gear]
  (lazy-cat [gear] (gen-gears (attached-gear gear))))

(defn reposition-gears
  "Moves attached gears based on the terminal point of the previous
  gears line from the radius"
  [gears]
  (loop [[gear & rest-gears] gears
         new-pos []
         result []]
    ;; (println new-pos)
    (if gear
      ;; If we have a gear, calculate and pass on to recur
      (let [next-new-pos (core/line-edge-coord gear)
            updated-gear (if (seq new-pos)
                           (assoc gear :position new-pos)
                           gear)]
        (recur rest-gears
               next-new-pos
               (conj result updated-gear)))
      result)))

;; Draw functions
(def initial-state (-> (random-gear mid-position)
                       gen-gears
                       (->> (take 8))))

(defn draw-gear [canvas {:keys [^double radius ^double position ^double line-angle] :as gear}]
  (let [[x y] position
        line-end-position (core/line-edge-coord gear)]
    (-> canvas
        (c/set-color [255 104 5])
        (c/ellipse x y 10 10)
        (c/set-color [0 0 0])
        (c/set-stroke 1)
        (c/ellipse x y (* 2 radius) (* 2 radius) true)
        (c/set-color [255 10 42])
        (c/line position line-end-position))))

(defn mark-canvas [canvas marks]
  (doseq [[x y] marks]
      (-> canvas
          (c/set-color [83 173 65])
          (c/ellipse x y 5 5))))

(defn draw
  [canvas window frame {:keys [gears ^doubles marks] :as state}]
  (c/set-background canvas [255 255 255])
  ;;draw gears
  (doseq [gear gears]
    (-> canvas
        (draw-gear gear)))
  ;;draw marks
  (-> canvas
      (mark-canvas marks))
  ;; update state
  (let [updated-marks (conj marks (core/line-edge-coord (last gears)))
        bound-marks (if (< (count marks) ^long (:max-marks options))
                      updated-marks
                      (into [] (rest updated-marks)))
        next-state
        (-> state
            (assoc :gears (->> gears
                               (map core/rotate)
                               reposition-gears))
            (assoc :marks bound-marks))]
    next-state))

(defn draw-proxy
  [& args]
  (apply draw args))

(def scene-canvas (apply c/canvas scene-size))
(def window (c/show-window {:canvas scene-canvas
                            :window-name "Crazy lines"
                            :w (get scene-size 0)
                            :h (get scene-size 1)
                            :draw-fn draw-proxy
                            :draw-state {:gears initial-state
                                         :marks []}}))

(.setAlwaysOnTop ^javax.swing.JFrame (:frame window) true)
