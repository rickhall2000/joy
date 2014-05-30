(ns joy.ch15)

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
