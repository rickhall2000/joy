(ns joy.ch15
  (:require [clojure.core.reducers :as r]))

(set! *warn-on-reflection* true)

(defn  ^Double asum-sq [ ^floats xs]
  (let [^floats dbl (amap xs i ret
                  (* (aget xs i)
                     (aget xs i)))]
    (areduce dbl i ret 0
             (+ ret (aget dbl i)))))

(time (dotimes [_ 10000]
        (asum-sq (float-array [1 2 3 4 5]))))

(time
 (.intValue (asum-sq (float-array [1 2 3 4 5]))))

;; 15.2 Tansients
(reduce merge [{1 3} {1 2} {3 4} {3 5}])

(defn zencat1 [x y]
  (loop [src y ret x]
    (if (seq src)
      (recur (next src) (conj ret (first src)))
      ret)))

(zencat1 [1 2 3] [4 5 6])

(time (dotimes [_ 1000000] (zencat1 [1 2 3] [4 5 6])))

(defn zencat2 [x y]
  (loop [src y ret (transient x)]
    (if src
      (recur (next src) (conj! ret (first src)))
      (persistent! ret))))

(zencat2 [1 2 3] [4 5 6])

(time (dotimes [_ 1000000] (zencat2 [1 2 3] [4 5 6])))

(def bv (vec (range 1e6)))

(first (time (zencat1 bv bv)))
(first (time (zencat2 bv bv)))

;; 15.3 Chunked Sequences
(def gimme #(do (print \.) %))

(take 1 (map gimme (range 32)))

(defn seq1 [s]
  (lazy-seq
   (when-let [[x] (seq s)]
     (cons x (seq1 (rest s))))))

(take 1 (map gimme (seq1 (range 32))))

;; 15.4 Memoization
(def gcd (memoize
          (fn [x y]
            (cond
             (> x y) (recur (- x y) y)
             (< x y) (recur x (- y x))
             :esle x))))

(gcd 1000645475 56130776629010010)

(defprotocol CacheProtocol
  (lookup [cache e])
  (has? [cache e])
  (hit [cache e])
  (miss [cache e ret]))

(deftype BasicCache [cache]
  CacheProtocol
  (lookup [_ item]
    (get cache item))
  (has? [_ item]
    (contains? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (BasicCache. (assoc cache item result))))

(def cache (BasicCache. {}))
(lookup (miss cache '(servo) :robot) '(servo))

(defn through [cache f item]
  (if (has? cache item)
    (hit cache item)
    (miss cache item (delay (apply f item)))))

(deftype PluggableMemoization [f cache]
  CacheProtocol
  (has? [_ item] (has? cache item))
  (hit [this item] this)
  (miss [_ item result]
    (PluggableMemoization. f (miss cache item result)))
  (lookup [_ item]
    (lookup cache item)))

(defn memoization-impl [cache-impl]
  (let [cache (atom cache-impl)]
    (with-meta
      (fn [& args]
        (let [cs (swap! cache through (.f cache-impl) args)]
          @(lookup cs args)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 3000) x))
(def sometimes-slowly (memoization-impl
                       (PluggableMemoization.
                        slowly
                        (BasicCache. {}))))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])

;; 15.5 Understanding Coercion

(defn factorial-a [original-x]
  (loop [x original-x acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(factorial-a 20)

(time (dotimes [_ 1e5] (factorial-a 20)))

(defn factorial-b [original-x]
  (loop [x (long original-x) acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(time (dotimes [_ 1e5] (factorial-b 20)))

(defn factorial-c [^long original-x]
  (loop [x original-x acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(time (dotimes [_ 1e5] (factorial-b 20)))

(set! *unchecked-math* true)
(defn factorial-d [^long original-x]
  (loop [x original-x acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (* x acc)))))

(time (dotimes [_ 1e5] (factorial-d 20)))

(set! *unchecked-math* false)
(time (dotimes [_ 1e5] (factorial-d 20)))

(factorial-d 21)
#_(factorial-a 21)

(defn factorial-e [^double original-x]
  (loop [x original-x acc 1.0]
    (if (>= 1.0 x)
      acc
      (recur (dec x) (* x acc)))))

(factorial-e 10.0)
(factorial-e 10)
(factorial-e 20.0)
(factorial-e 30.0)
(factorial-e 171.0)

(time (dotimes [_ 1e5] (factorial-e 20.0)))

(defn factorial-f [^long original-x]
  (loop [x original-x acc 1]
    (if (>= 1 x)
      acc
      (recur (dec x) (*' x acc)))))

(factorial-f 20)
(factorial-f 30)
(factorial-f 171 )

(time (dotimes [_ 1e5] (factorial-f 20)))

;; 15.6 Reducibles

(defn empty-range? [start end step]
  (or (and (pos? step) (>= start end))
      (and (neg? step) (<= start end))))

(defn lazy-range [i end step]
  (lazy-seq
   (if (empty-range? i end step)
     nil
     (cons i
           (lazy-range (+ i step)
                       end
                       step)))))

(lazy-range 5 10 2)
(lazy-range 6 0 -1)

(reduce conj [] (lazy-range 6 0 -1))

(reduce + 0 (lazy-range 6 0 -1))


(defn reducible-range [start end step]
  (fn [reducing-fn init]
    (loop [result init i start]
      (if (empty-range? i end step)
        result
        (recur (reducing-fn result i)
               (+ i step))))))

(def countdown-reducible (reducible-range 6 0 -1))
(countdown-reducible conj [])
(countdown-reducible + 0)

(defn half [x] (/ x 2))

(defn sum-half [result input]
  (+ result (half input)))

(reduce sum-half 0 (lazy-range 0 10 2))
((reducible-range 0 10 2) sum-half 0)

(defn half-transformer [f1]
  (fn f1-half [result input]
    (f1 result (half input))))

((reducible-range 0 10 2) (half-transformer +) 0)

((reducible-range 0 10 2) (half-transformer conj) [])

(defn mapping [map-fn]
  (fn map-transformer [f1]
    (fn [result input]
      (f1 result (map-fn input)))))

((reducible-range 0 10 2) ((mapping half) +) 0)

(defn filtering [filter-pred]
  (fn [f1]
    (fn [result input]
      (if (filter-pred input)
        (f1 result input)
        result))))

((reducible-range 0 10 2) ((filtering #(not= % 2)) conj) [] )

((reducible-range 0 10 2)
 ((filtering #(not= % 2))
  ((mapping half) conj))
 [])

((reducible-range 0 10 2)
 ((mapping half)
  ((filtering #(not= % 2)) conj))
 [])

(defn mapcatting [map-fn]
  (fn [f1]
    (fn [result input]
      (let [reducible (map-fn input)]
        (reducible f1 result)))))

(defn and-plus-ten [x]
  (reducible-range x (+ 11 x) 10))

((and-plus-ten 5) conj [])

((reducible-range 0 10 2) ((mapcatting and-plus-ten) conj) [])

(filter #(not= % 2)
        (map half
             (lazy-range 0 10 2)))

(defn r-map [mapping-fn reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((mapping mapping-fn) reducing-fn) init)))

(defn r-filter [filter-pred reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((filtering filter-pred) reducing-fn) init)))

(def our-final-reducible
  (r-filter #(not= % 2)
            (r-map half
                   (reducible-range 0 10 2))))

(our-final-reducible conj [])

(defn core-r-map [mapping-fn core-reducible]
  (r/reducer core-reducible (mapping mapping-fn)))

(defn core-r-filter [filter-pred core-reducible]
  (r/reducer core-reducible (filtering filter-pred)))

(reduce conj []
        (core-r-filter #(not= % 2)
                       (core-r-map half [0 2 4 6 8])))

(defn reduce-range [reducing-fn init start end step]
  (loop [result init i start]
    (if (empty-range? i end step)
      result
      (recur (reducing-fn result i)
             (+ i step)))))

#_(defn core-reducible-range [start end step]
  (reify protos/CollReduce
    (coll-reduce [this reducing-fn init]
      (reducing-range reducing-fn init start end step))
    (coll-reduce [this reducing-fn]
      (if (empty-range? start end step)
        (reducing-fn)
        (reduce-range reducing-fn start (+ start step) end step)))))

#_(reduce conj []
        (core-r-filter #(not= % 2)
                       (core-r-map half
                                   (core-reducible-range 0 10 2))))

(reduce + [1 2 3 4 5])
(r/fold + [1 2 3 4 5])

(defn core-f-map [mapping-fn core-reducible]
  (r/folder core-reducible (mapping mapping-fn)))

(defn core-f-filter [filter-pred core-reducible]
  (r/folder core-reducible (filtering filter-pred)))

(r/fold +
        (core-f-filter #(not= % 2)
                       (core-f-map half
                                   [0 2 4 6 8])))

(r/fold +
        (r/filter #(not= % 2)
                  (r/map half
                         [0 2 4 6 8])))

(r/fold + (range 10))
(r/fold + (r/filter odd? (range 10)))

(r/fold (fn ([] 100) ([a b] (+ a b))) (range 10))

(r/fold (r/monoid + (constantly 100)) (range 10))

(r/fold 512
        (r/monoid + (constantly 100))
        +
        (range 10))

(r/fold 4 (r/monoid conj (constantly []))
        conj (vec (range 10)))

(r/fold 4 (r/monoid into (constantly []))
        conj (vec (range 10)))

(r/foldcat (r/filter even? (vec (range 1000))))

(seq (r/foldcat (r/filter even? (vec (range 10)))))

(def big-vector
  (vec (range 0 (* 10 1000 1000) 2)))

(time
 (r/fold + (core-f-filter even?
                          (core-f-map half big-vector))))
