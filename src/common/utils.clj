(ns common.utils
  (:require [fastmath.core :as m]))

(defn proxy-fn
  "Returns a function that proxies `draw-fn`"
  [draw-fn]
  (fn [& args]
    (apply draw-fn args)))


(defn pi [factor] (* m/PI factor))

(def pi-mem (memoize pi))

(defmacro map-keyed
  {:style/indent :defn}
  [& args]
  `(->> (list ~@args)
        (interleave (map keyword '(~@args)))
        (apply hash-map)))
