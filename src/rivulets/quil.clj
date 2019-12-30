(ns rivulets.quil
  (:require [rivulets.core :as riv :refer [pi]]
            [quil.core :as q :include-macros true]
            [clojure.datafy :as datafy]
            [fastmath.random :as random]
            [quil.middleware :as m]
            [com.rpl.specter :refer [transform ALL]]
            [fastmath.core :as math]))

(def width 800)
(def height 800)
(def half-width (/ width 2))
(def half-height (/ height 2))

#_(if-let [{:keys [a b]} nil]
    'yes 'no)
;;=====DRAW=====
(defn- spike-rad-offset
  [{:keys [origin radius angle start-angle spike] :as res-circ}]
  (if-let [{:keys [max-arc-length start-angle max-radius]} spike]
    (-> (- (* 2 angle) max-arc-length (* 2 start-angle))
        (math/pow 2)
        (/ (math/pow max-arc-length 2))
        -
        inc
        (* (+ max-radius radius)))))

(defn draw-state [state]
  (q/background 0)
  (let [res-circ (:res-circ state) 
        {:keys [origin radius start-angle angle max-length spike]} res-circ
        [x y] origin
        mid-angle (-> (+ angle start-angle) (/ 2))]
    (q/stroke [0 255 0])
    (q/stroke-weight 1)
    (doseq [a (range start-angle angle 0.003)]
      (let [radius' (if (and spike (>= a (:start-angle spike)))
                      (-> res-circ
                          spike-rad-offset
                          (* 0.5)
                          (+ radius))
                      radius)]
        (apply q/point (riv/circle-point origin a radius')))
      #_(cond
          (nil? spike) (apply q/point (riv/circle-point origin a radius))
          :else (apply q/point (spike-coord res-circ))))))

;;=====STATE=====
(defn setup []
  (q/frame-rate 30)
  (q/background 255 255 255)
  (q/set-state! #_:rivulets #_(list (riv/make-rivulet [half-width 0]
                                                      (riv/pi 1.5)
                                                      width
                                                      height
                                                      {:sweep (riv/pi 1/8)
                                                       :length 5}))
                :res-circ  {:origin [300 300] 
                            :radius 250
                            :start-angle 0
                            :angle 0
                            :max-length 300
                            :spike {:start-angle 0
                                    :max-arc-length (pi 1/4)
                                    :max-radius 250}}))

(defn key-pressed [event state]
  (let [{:keys [start-angle angle spike radius]} state]
    (when (not spike)
      (assoc state :spike {:start-angle angle 
                           :max-arc-length (pi 1/8)
                           :max-radius (+ radius 50)}))))

(defn- next-spike
  [{:keys [start-angle max-radius max-arc-length] :as spike} current-angle]
  (when (or (nil? spike)
            (< (- current-angle start-angle)
               max-arc-length))
    spike))

(defn- next-res-cirle
  [{:keys [radius start-angle angle max-length spike] :as res-circ}]
  (let [length-incr 0.02 ;;incr by time factor
        next-angle (-> angle (+ length-incr) #_(mod (pi 2)))
        next-start-angle (if (< next-angle start-angle)
                           0 start-angle)
        arc-length (-> (- next-angle next-start-angle)
                       (* radius))]
    ;; reached length, adjust start-angle
    (assoc res-circ
           :angle next-angle
           :start-angle (if (>= arc-length max-length)
                          (+ next-start-angle length-incr)
                          next-start-angle)
           :spike (next-spike spike next-angle))))

(defn update-state [state]
  (-> state
      (update :res-circ next-res-cirle)))

(q/defsketch rivulet-quil-example
  :title "rivulets in Quil"
  :setup setup,
  :draw draw-state
  :update update-state
  :key-pressed key-pressed
  :size [800 600]
  :features [:keep-on-top]
  :middleware [m/fun-mode])

(comment
  (cons [1 2] '([3 4] [5 6]))
  (transform [:lst ALL] inc {:lst [1 2 3 4]})
  (transform [ALL ALL] inc [[1 2] [3 4]])
  (transform [ALL ALL] inc ['(1 2) '(3 4)])
  (transform [ALL ALL] inc '([1 2] (3 4)))
  #_(partition 2 1 (list (pi 2)) (take 5 (iterate #(+ % (/ (pi 2) 4)) 0)))
  #_(partition 2 1 (list (pi 2)) (range 0 (pi 2) (-> (pi 2) (/ 3))))
  #_(partition 2 1 '(5) (range 0 5))

  ;; Create a line drawing effect 
  (let [[x1 y1] [(/ width 2) 0]
        [x2 y2] [(/ width 2) height]
        dx (- x2 x1)
        dy (- y2 y1)
        time (t 0.0001)]
    (q/fill 255 0 0)
    (q/line x1
            y1
            (+ x1 (* time dx))
            (+ y1 (* time dy))))
  #_(defn draw-state [state]
      (doseq [rivulet (:rivulets state)
              :let [{:keys [x y point-seq]} rivulet
                    [x1 y1] (first point-seq)]]
        (q/fill 255 0 0)
        (q/line x1 y1 x y)))

  #_(def circle (riv/make-coil-circle [200 200] 100 200))

  )

