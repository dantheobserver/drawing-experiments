(ns common.utils
  (:require [fastmath.core :as m]))

(defmacro anon-proxy
  [draw-fn]
  `(fn [& ~'args]
     (apply ~draw-fn ~'args)))

(defn pi [factor] (* m/PI factor))

(def pi-mem (memoize pi))

(defmacro map-keyed
  {:style/indent :defn}
  [& args]
  `(->> (list ~@args)
        (interleave (map keyword '(~@args)))
        (apply hash-map)))

(defn projected-point
  "Given a point with `x`,`y`,`z` and
  a vanishing point with ``"
  [[x y z] [xv yv zv]]
  (cond
    (= z 0) [x y]
    (= z zv) [xv yv zv]
    :else (let [dist (m/dist x y xv yv)
                dx (- xv x)
                dy (- yv y)
                pt-dist (* z (/ dist zv))
                ratio (/ pt-dist dist)
                offsets (mapv (partial * ratio) [dx dy])]
            (mapv + [x y] offsets))))

(comment
  (let [draw-fn (fn [] 1)
        aproxy (anon-proxy draw-fn)]
    #_(macroexpand '(anon-proxy draw-fn))
    (aproxy))
  (let [test (fn [] (println "testing"))
        a-proxy (anon-proxy proxy-fn)]
    (a-proxy)))
