(ns joy.ch6)

(defn xconj [t v]
  (cond
   (nil? t) {:val v :L nil :R nil}
   (< v (:val t)) {:val (:val t) :L (xconj (:L t) v) :R (:R t)}
   :default {:val (:val t) :L (:L t) :R (xconj (:R t) v)}))

(defn xconj' [t v]
  (cond
   (nil? t) {:val v :L nil :R nil}
   (< v (:val t)) (assoc t :L (xconj (:L t) v))
   :default (assoc t :R (xconj (:R t) v))))

(defn xseq [t]
  (when t
    (concat (xseq (:L t)) [(:val t)] (xseq (:R t)) )))


(xseq
 (xconj'
  (xconj'
   (xconj' nil 3) 2) 5))

(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(def tri-nums (map triangle (iterate inc 1)))

(take 10 (filter even? tri-nums))

(defn rand-ints [n]
  (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list*
                 (filter smaller? xs)
                 pivot
                 (remove smaller? xs)
                 parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))

(qsort (rand-ints 20))
