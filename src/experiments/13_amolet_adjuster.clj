(ns experiments.13-amolet-adjuster
  (:require [clojure2d.core :as c]
            [clojure2d.pixels :a p]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [common.utils :as utils]
            [fastmath.core :as math])
  #_(:import [javax.swing BoxLayout Box]))

(defun draw [canvas window frame state]
  )

(let [w 500
      h 500
      fps 60
      canvas (c/canvas w h)
      window (c/show-window canvas "amoled-adjuster" #(draw %1 %2 %3 %4))])
