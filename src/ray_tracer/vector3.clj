(ns ray-tracer.vector3)

(defn product [v1 v2]
  (mapv * v1 v2))

(defn squared-length [vec3]
  (reduce + (product vec3 vec3)))

(defn length [vec3]
  (Math/sqrt (squared-length vec3)))

(defn plus [v1 v2]
  (mapv + v1 v2))

(defn minus [v1 v2]
  (mapv - v1 v2))

(defn division [v1 v2]
  (mapv / v2 v2))

(defn scalar-multiply [s vec3]
  (mapv #(* s %) vec3))

(defn dot-product [v1 v2]
  (reduce + (product v1 v2)))

(defn cross-product [[a1 a2 a3] [b1 b2 b3]]
  [(- (* a2 b3) (* a3 b2))
   (- (* a3 b1) (* a1 b3))
   (- (* a1 b2) (* a2 b1))])

(defn unit-vector [vec3]
  (mapv #(/ % (length vec3)) vec3))
