(ns joy.ch7)

(defn fnth [n]
  (apply comp
         (cons first
               (take (dec n) (repeat rest)))))

((fnth 5) [1 2 3 4 5 6])

(map (comp
      keyword
      #(.toLowerCase %)
      name)
     '(a B C))

(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(defn slope' [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (-  (p2 1) (p1 1))
     (- (p2 0) (p1 0))))

(defn put-things [m]
  (into m {:meat "beef" :veggie "broccoli"}))

(put-things {})

(defn vegan-constraints [f m]
  {:pre [{:veggie m}]
   :post [{:veggie %} (nil? {:meat %})]}
  (f m))

(defn balanced-diet [f m]
  {:post [{:meat %} {:veggie %}]}
  (f m))

(defn finicky [f m]
  {:post [(= :meat %) (:meat m)]}
  (f m))

(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))

(def bearings [{:x 0 :y 1}
               {:x 1 :y 0}
               {:x 0 :y -1}
               {:x -1 :y 0}])

(defn forward [x y bearing-num]
  [(+ x (:x (bearings bearing-num)))
   (+ y (:y (bearings bearing-num)))])

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing ([:north :east :south :west] bearing-num)
   :forward (fn [] (bot (+ x (:x (bearings bearing-num)))
                       (+ y (:y (bearings bearing-num)))
                       bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left (fn [] (bot x y (mod (- 1 bearing-num) 4)))})

(:bearing ((:forward ((:forward ((:turn-right (bot 5 5 0))))))))


;; 7.3 recursion
(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})

(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(defn elevator [commands]
  (letfn
      [(ff-open [[_ & r]]
         #(case _
            :close (ff-closed r)
            :done true
            false))
       (ff-closed [[_ & r]]
         #(case _
            :open (ff-open r)
            :up (sf-closed r)
            false))
       (sf-closed [[_ & r]]
         #(case _
           :down (ff-closed r)
           :open (sf-open r)
           false))
       (sf-open [[_ & r]]
         #(case _
            :close (sf-closed r)
            :done true
            false))]
    (trampoline ff-open commands)))

(defn fac-cps [n k]
  (letfn [(cont [v] (k (* v n)))]
    (if (zero? n)
      (k 1)
      (recur (dec n) cont))))

(defn fac [n]
  (fac-cps n identity))

(defn mk-cps [accept? kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (fn [v]
                    (k ((partial kont v) n)))]
         (if (accept? n)
           (k 1)
           (recur (dec n) cont))))
     n kend)))

(def fac
  (mk-cps zero?
          identity
          #(* %1 %2)))

(def world [[1 1 1 1 1]
            [999 999 999 999 1]
            [1 1 1 1 1]
            [1 999 999 999 999]
            [1 1 1 1 1]])

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
     (filter (fn [new-yx]
               (every? #(< -1 % size) new-yx))
             (map #(vec (map + yx %))
                  deltas))))

(defn estimate-cost [step-cost-est size y x]
  (* step-cost-est
     (- (+ size size) y x 2)))

(defn path-cost [node-cost cheapest-nbr]
  (+ node-cost
     (or (:cost cheapest-nbr) 0)))

(defn total-cost [newcost step-cost-est size y x]
  (+ newcost
     (estimate-cost step-cost-est size y x)))

(defn min-by [f coll]
  (when (seq coll)
    (reduce (fn [min other]
              (if (> (f min) (f other))
                other
                min))
            coll)))

(min-by :cost [{:cost 100} {:cost 36} {:cost 9}])

(defn astar [start-yx step-est cell-costs]
  (let [size (count cell-costs)]
    (loop [steps 0
           routes (vec (replicate size (vec (replicate size nil))))
           work-todo (sorted-set [0 start-yx])]
      (if (empty? work-todo)
        [(peek (peek routes)) :steps steps]
        (let [[_ yx :as work-item] (first work-todo)
              rest-work-todo (disj work-todo work-item)
              nbr-yxs (neighbors size yx)
              cheapest-nbr (min-by :cost
                                   (keep #(get-in routes %)
                                         nbr-yxs))
              newcost (path-cost (get-in cell-costs yx)
                                 cheapest-nbr)
              oldcost (:cost (get-in routes yx))]
          (if (and oldcost (>= newcost oldcost))
            (recur (inc steps) routes rest-work-todo)
            (recur (inc steps)
                   (assoc-in routes yx
                             {:cost newcost
                              :yxs (conj (:yxs cheapest-nbr [])
                                         yx)})
                   (into rest-work-todo
                        (map
                         (fn [w]
                           (let [[y x] w]
                             [(total-cost newcost step-est size y x) w]))
                         nbr-yxs)))))))))



(astar [0 0] 900 world)
