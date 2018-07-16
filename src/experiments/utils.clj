(ns experiments.utils.core)

(def pico-8-colors
  {:black "#000000"
   :dark-blue "#1D2B53"
   :dark-purple "#7E2553"
   :dark-green "#008751"
   :brown "#AB5236"
   :dark-gray "#5F574F"
   :light-gray "#C2C3C7"
   :white "#FFF1E8"
   :red "#FF004D"
   :orange "#FFA300"
   :yellow "#FFEC27"
   :green "#00E436"
   :blue "#29ADFF"
   :indigo "#83769C"
   :pink "#FF77A8"
   :peach "#FFCCAA"})

(def pico-8-color-names
  (keys pico-8-colors))

(defn pico-8-c [color]
  (condp = (type color)
    clojure.lang.Keyword (color pico-8-colors)
    java.lang.Long (nth (vals pico-8-colors) color)
    java.lang.String (get pico-8-colors (keyword color))))
