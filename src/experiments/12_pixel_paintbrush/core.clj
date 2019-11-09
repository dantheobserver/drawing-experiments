(ns experiments.12-pixel-paintbrush.core
  (:require [clojure2d.core :as c]
            [clojure.core.reducers :as reducers]
            [common.utils :refer [update-state! mouse-pos]]
            [experiments.12-pixel-paintbrush.elements :refer [update-points! draw-frame ->PulsePixel]]
            [clojure2d.pixels :as p]))

(set! *unchecked-math* :warn-on-boxed)

(def pallate {:background :black
              :pointer :white})

(defn draw-mode-indicator
  [canvas mode color]
  (-> canvas
      (c/set-color color)
      (c/text (name mode) 12 12)))

(defn draw-pointer
  [canvas [^int px ^int py] ^double size color]
  (-> canvas
      (c/set-color color)
      (c/line px py px (- py size))
      (c/line px py px (+ py size))
      (c/line px py (- px size) py)
      (c/line px py (+ px size) py)))

;;1px/60f
(defn draw [canvas window frame state]
  (let [{:keys [pointer points draw-mode]} (c/get-state window)]
    (println points)

    ;;background and cursor
    (-> canvas
        (c/set-background (:background pallate))
        (draw-mode-indicator draw-mode [255 255 255])

        ;;pointer
        (draw-pointer pointer 12 [255 255 255]))

    (doseq [point points]
      (draw-frame point canvas [255 255 255])))

  (update-points! window frame)
  state)

(let [w 800
      h 600
      fps 60
      name "night-lights"
      window (c/show-window {:window-name name
                             :canvas (c/canvas w h)
                             :always-on-top? true
                             :state {:pointer [0 0]
                                     :draw-mode :pulse
                                     :points []}
                             :fps fps
                             ;; :draw-state {:pointer [0 0]}
                             :draw-fn #(draw %1 %2 %3 %4)})]

  (defmethod c/mouse-event
    [name :mouse-moved] [evt state]
    (assoc state :pointer (mouse-pos evt)))

  (defmethod c/mouse-event
    [name :mouse-clicked] [evt state]
    (let [[x y] (mouse-pos evt)]
      (update state :points conj (->PulsePixel x y 0))))

  #_(defmethod c/key-event
      [name :key-pressed] [evt state]
      (move-pointer (c/key-code evt) state)
      ))

