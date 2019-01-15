(ns common.utils
  (:require [fastmath.core :as m]))

(defmacro anon-proxy
  "Creates a function that proxies another fn.
  Used when live coding with an api that keeps
  a draw fn reference."
  [draw-fn]
  `(fn [& ~'args]
     (apply ~draw-fn ~'args)))


(defmacro map-keyed
  "given bound symbol names, will return a map
  of keys with the symbol name and its value."
  {:style/indent :defn}
  [& args]
  `(->> (list ~@args)
        (interleave (map keyword '(~@args)))
        (apply hash-map)))

(defn pi [factor] (* m/PI factor))

(def pi-mem (memoize pi))

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
    (aproxy))

  (let [test (fn [] (println "testing"))
        a-proxy (anon-proxy proxy-fn)]
    (a-proxy))

  (let [some-promise (promise)]
    (println "Calling thread to print promise")
    (.start (Thread. (fn [] (println "unblocking thread" @some-promise))))
    (deliver some-promise 12)
    ))
