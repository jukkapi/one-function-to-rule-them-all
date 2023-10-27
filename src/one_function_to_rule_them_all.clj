(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (let [jn (fn [a s] (str a " " s))]
        (reduce jn a-seq))))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) []
  (let [cn (fn [a s] (conj a x s))]
    (reduce cn [(first a-seq)] (rest a-seq)))))

(defn my-count [a-seq]
  (let [i (fn [a s] (inc a))]
    (reduce i 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [min-max (fn [a s] [(min (get a 0) s) (max (get a 1 ) s)])]
    (reduce min-max [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq) [n]
    (loop [f [(first sorted-seq)]
           r (rest sorted-seq)]
      (cond
        (<= n (first f))
          (concat [n] f r)
        (empty? r) (conj f n)
        (and (<= (first f) n) (>= (first r) n))
          (concat f [n] r)
        :else (recur (conj f (first r)) (rest r))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus
  ([x] (- 0 x))
  ([x y] (- x y)))


(defn count-params [& more]
  (count (concat more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))



(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p1 p2] (fn [x] (and (p1 x) (p2 x))))
  ([p1 p2 & more]
    (reduce pred-and (concat [p1 p2] more))))


(defn my-map [f a-seq]
  [:-])

