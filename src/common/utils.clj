(ns common.utils
  (:require [clojure2d.core :as c]
            [fastmath.core :as m]
            [com.rpl.specter :as sp :refer-macros [select transform]]))

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
  a vanishing point with `x-vp` `y-vp` `z-vp`
  give a 2d projection of that point"
  [[x y z :as point] [x-vp y-vp z-vp :as vanishing-point]]
  (cond
    (= z 0) [x y]
    (= z z-vp) [x-vp y-vp z-vp]
    :else (let [dist (m/dist x y x-vp y-vp)
                dx (- x-vp x)
                dy (- y-vp y)
                pt-dist (* z (/ dist z-vp))
                ratio (/ pt-dist dist)
                offsets (mapv (partial * ratio) [dx dy])]
            (mapv + [x y] offsets))))

(defn projected-points
  "Takes in `paths-vec' which is a vector of 3D vectors
  and a `vanishing-point' and returns a vector of projected
  2D vectors.
  "
  [vanishing-point point-vecs]
  (sp/transform [sp/ALL sp/ALL]
                #(utils/projected-point % vanishing-point)
                point-vecs))

;; Comment section
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

(defn update-state! [window key fn]
  (let [cur-state (c/get-state window)]
    (c/set-state! window (update cur-state :key fn))))
