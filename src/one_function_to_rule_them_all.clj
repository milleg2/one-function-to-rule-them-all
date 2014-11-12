(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
    (reduce (fn [x y] (str x " " y)) a-seq)))

(defn my-interpose [x a-seq]
  (reduce (fn [y z]
            (if (empty? y)
              (conj y z)
              (conj (conj y x) z)))
          [] a-seq))

(defn my-count [a-seq]
  (let [counter (fn [count x]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (let [reverser (fn [r-seq x]
                   (conj r-seq x))]
    (reduce reverser '() a-seq)))

(defn min-max-element [a-seq]
  (let [min-maxer (fn [[min max] x]
                    (cond
                      (nil? min) [x x]
                      (< x min) [x max]
                      (> x max) [min x]
                      :else [min max]))]
    (reduce min-maxer [] a-seq)))

(defn insert [sorted-seq n]
  (loop [r-seq sorted-seq
         build []]
    (let [f (first r-seq)
          r (rest r-seq)]
      (cond
        (empty? r-seq) (conj build n)
        (<= n f) (into (conj (conj build n) f) r)
        :else (recur r (conj build f))))))

(defn insertion-sort [a-seq]
  (let [sorter (fn [sorted x]
                 (insert sorted x))]
    (reduce sorter [] a-seq)))

(defn parity [a-seq]
  (let [toggle (fn [s x]
                 (if (contains? s x)
                   (disj s x)
                   (conj s x)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& more]
  (let [counter (fn [count x]
                  (inc count))]
    (reduce counter 0 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (* x y) more)))

(defn pred-and 
  ([]
   (fn [x] true))
  ([x]
   x)
  ([x y]
   (fn [z] (and (x z) (y z))))
  ([x y & more] 
   (reduce pred-and (pred-and x y) more)))

(defn my-map-inner [f a-seq]
  (loop [build []
         r-seq a-seq]
    (if (empty? r-seq)
      build
      (let [fi (first r-seq)
            r (rest r-seq)]
        (recur (conj build (apply f fi)) r)))))

(defn my-map [f & more]
  (let [args (reduce
               (fn [curr-seq x]
                 (if (empty? curr-seq)
                   (loop [build []
                          r-seq x]
                     (if (empty? r-seq) build
                       (recur (conj build [(first r-seq)])
                              (rest r-seq))))
                   (loop [build []
                          f-curr-seq (first curr-seq)
                          r-curr-seq (rest curr-seq)
                          r-seq x]
                     (if (nil? f-curr-seq) build
                       (recur (conj build (conj f-curr-seq (first r-seq)))
                              (first r-curr-seq)
                              (rest r-curr-seq)
                              (rest r-seq))))))
               [] more)]
    (my-map-inner f args)))

