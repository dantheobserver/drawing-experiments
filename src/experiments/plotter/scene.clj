(ns experiments.plotter.scene
  (:require [experiments.plotter.core :as core]
            [fastmath.core :as m]
            [clojure2d.core :as c]
            [com.rpl.specter
             :as sp
             :refer [transform multi-transform multi-path terminal terminal-val
                     ALL LAST]]))

(defn draw-point
  [canvas {:keys [coord ray-coord term-point] :as point-ray} state]
  (let [[x y] coord]
    (-> canvas
        ;;draw-point
        (c/set-color [0 0 255])
        (c/ellipse x y 4 4)
        ;;draw-ray
        (c/set-color [255 0 0])
        (c/line coord ray-coord))))

(defn draw-plotted-points
  "Draw the vector of points plotted"
  [canvas plotted-points state]
  (doseq [point plotted-points]
    (draw-point canvas point state))
  canvas)

(defn draw
  [canvas window frame state]
  (let [{:keys [plotted-points] :as global-state} (c/get-state window)]
    #_(println "Drawing" plotted-points)
    (-> canvas
        (c/set-background :white)
        (draw-plotted-points plotted-points state))
    (c/set-state! window (transform [:plotted-points ALL]
                                    #(core/next-state % global-state)
                                    global-state)))
  state)

(let [w 500
      h 500
      fps 60
      window-name "plotter"]
  (def window (c/show-window
               {:window-name window-name 
                :canvas (c/canvas w h)
                :fps fps
                :state {:plotted-points []}
                :draw-fn #(draw %1 %2 %3 %4)})))

(def ray-speed 0.05)

(defn ray-vec
  "Get the slope vector taking into account speed"
  [coord next-coord]
  (->> (mapv - next-coord coord)
       (mapv #(* % ray-speed))))

(defn add-point
  [points {:keys [coord] :as point}]
  (if (empty? points)
    (conj points point)
    ;; Set ray-movement-vector and termination-point of vector before inserted
    (conj (multi-transform [LAST
                            (multi-path 
                             [:term-point (terminal-val coord)]
                             [(terminal #(assoc % :ray-vec (ray-vec (:coord %) coord)))])]
                           points)
          point)))

(defmethod c/mouse-event ["plotter" :mouse-clicked] [evt state]
  (println "*********************")
  (clojure.pprint/pprint (:plotted-points state))
  (let [coord ((juxt c/mouse-x c/mouse-y) evt)]
    (update state
            :plotted-points
            add-point
            (core/->PointRay coord coord [0 0] nil))))

;;-------------;;
;;   comments  ;;
;;-------------;;
(comment 
  (let [{[x y] :a :as test} {:a [12 11]}]
    [x y test])

  (update {:a 1}
          :a
          + 12)

  ((juxt + -) 1)

  (transform [:a :b] inc {:a {:b 1}})
  (multi-transform [:a :b (multi-path [:a (terminal inc)]
                                      [:b (terminal dec)])]
                   {:a {:b {:a 1 :b 2}}})

  (multi-transform [LAST
                    (multi-path
                     ;;Can't reference LAST from a terminal path directly
                     [(terminal #(assoc % :a (+ 12 (:b %))))]
                     [:b (terminal dec)]
                     [:c (terminal-val 123)])]
                   [1 2 3 4 5 {:a 1 :b 5 :c nil}])

  (sp/setval [:b sp/END] [ 1 ] {:a 12 :b [1 2 3]})
  )
