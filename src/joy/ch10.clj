(ns joy.ch10
  (:refer-clojure :exclude [aget aset count seq])
  (:require [clojure.core :as clj])
  (:import  [java.util.concurrent.Executors]
            [java.util.concurrent.locks.ReentrantLock]))

(def thread-pool (Executors/newFixedThreadPool
                  (+ 2 (.availableProcessors
                        (Runtime/getRuntime)))))

(defn dothreads!
  [f & {thread-count :threads
        exec-count :times
        :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit thread-pool
             #(dotimes [_ exec-count] (f)))))

(def initial-board
  [[:- :k :-]
   [:- :- :-]
   [:- :K :-]])

(defn board-map [f board]
  (vec (map #(vec (for [s %] (f s)))
            board)))

(defn board-map' [board]
  (vec (map #(vec (for [s %] (str s))) board)))
;; rewriting, to see what it does


(defn reset-board!  []
  (def board (board-map ref initial-board))
  (def to-move (ref [[:K [2 1]] [:k [0 1]]]))
  (def num-moves (ref 0)))

(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]]
                        size
                        yx))
  ([deltas size yx]
     (filter (fn [new-yx]
               (every? #(< -1 % size) new-yx))
             ;; prefix notation makes this less obvious.
             ;; infix -1 < % < size
             (map #(vec (map + yx %))
                  deltas))))

(def king-moves
  (partial neighbors
           [[-1 -1] [-1 0] [-1 1] [0 1] [1 -1] [1 0] [1 1]] 3))
;; deltas has 8 options because kings can move diagonals.  The queen vector would be really long!

(defn good-move?
  [to enemy-sq]
  (when (not= to enemy-sq)
    to))

(defn choose-move
  [[[mover mpos] [_ enemy-pos]]]
  [mover (some #(good-move? % enemy-pos)
               (shuffle (king-moves mpos)))])

(defn place [from to] to)
;; this replaces the contents of a cell,
;; when :k moves away from a cell, that cell
;; goes from having :k to having :-

(defn move-piece [[piece dest] [[_ src] _]]
  (alter (get-in board dest) place piece)
  (alter (get-in board src) place :-)
  (alter num-moves inc))

(defn update-to-move [move]
  (alter to-move #(vector (second %) move)))

(defn make-move []
  (let [move (choose-move @to-move)]
    (dosync (move-piece move @to-move))
    (dosync (update-to-move move))))

(dothreads! make-move :threads 100 :times 100)

;; 10.2
(defn make-move-v2 []
      (dosync
       (let [move (choose-move @to-move)]
         (move-piece move @to-move)
         (update-to-move move))))
(comment
  (defn move-piece [[piece dest] [[_ src] _]]
    (commute (get-in board dest) place piece)
    (commute (get-in board src) place :-)
    (commute num-moves inc))

  (defn update-to-move [move]
    (commute to-move #(vector (second %) move)))

  )

(defn stress-ref [r]
  (let [slow-tries (atom 0)]
    (future
      (dosync
       (swap! slow-tries inc)
       (Thread/sleep 200)
       @r)
      (println (format "r is: %s, history: %d, after %d tries" @r (.getHistoryCount r) @slow-tries)))
    (dotimes [i 500]
      (Thread/sleep 10)
      (dosync (alter r inc)))
    :done))

;; 10.3 - agents

(def joy (agent []))
(send joy conj "first edition")
(defn slow-conj [coll item]
  (Thread/sleep 1000)
  (conj coll item))

(send joy slow-conj "Second edition")

(def log-agent (agent 0))

(defn do-log [msg-id message]
  (println msg-id ":" message)
  (inc msg-id))

(defn do-step [channel message]
  (Thread/sleep 1)
  (send-off log-agent do-log (str channel message)))

(defn three-step [channel]
  (do-step channel " ready to begin (step 0)")
  (do-step channel " warming up (step 1)")
  (do-step channel " really getting going now (step 2)")
  (do-step channel " done (step 3)"))

(defn all-together-now []
  (dothreads! #(three-step "alpha"))
  (dothreads! #(three-step "beta"))
  (dothreads! #(three-step "gamma")))

(defn exercise-agents [send-fn]
  (let [agents (map #(agent %) (range 10))]
    (doseq [a agents]
      (send-fn a (fn [_] (Thread/sleep 1000))))
    (doseq [a agents]
      (await a))))

(time (exercise-agents send-off))
(time (exercise-agents send))

(send log-agent (fn [] 2000))

@log-agent

(agent-error log-agent)
(send log-agent (fn [_] 3000))

(restart-agent log-agent 2500 :clear-actions true)

(send-off log-agent do-log "The agent lives")

(defn handle-log-error [the-agent the-err]
  (println "an action sent to the log-agent threw" the-err))

(set-error-handler! log-agent handle-log-error)

(set-error-mode! log-agent :continue)

;; 10.4
(def ^:dynamic *time* (atom 0))
(defn tick [] (swap! *time* inc))
(dothreads! tick :threads 1000 :times 100)
@*time*

(defn manipulable-memoize [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

;; find returns a key value pair, if exits
(find {:a 1 :b 2} :a) ;; => [:a 1]
(find {:a 1 :b 2} :c) ;; => nil

(def slowly (fn [x] (Thread/sleep 1000) x))

(time [(slowly 9) (slowly 9)])

(def sometimes-slowly (manipulable-memoize slowly))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])

(meta sometimes-slowly)
(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(108)))

;; 10.5 Locks
(defprotocol SafeArray
  (aset [this i f])
  (aget [this i])
  (count [this])
  (seq [this]))

(defn make-dumb-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i] (clj/aget a i))
      (aset [this i f]
        (clj/aset a
                  i
                  (f (aget this i)))))))

(defn pummel [a]
  (dothreads! #(dotimes [i (count a)] (aset a i inc))
              :threads 100))

(def D (make-dumb-array Integer/TYPE 8))

(pummel D)

(seq D)

(defn make-safe-array [t sz]
  (let [a (make-array t sz)]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (locking a
          (clj/aget a i)))
      (aset [this i f]
        (locking a
          (clj/aset a
                    i
                    (f (aget this i))))))))

(def A (make-safe-array Integer/TYPE 8))

(pummel A)

(seq A)

(defn lock-i [target-index num-locks]
  (mod target-index num-locks))

;; not sure why it can't find reentrantlock, it doesn't have trouble with the import statement
#_(defn make-smart-array [t sz]
  (let [a (make-array t sz)
        Lsz (/ sz 2)
        L (into-array (take Lsz
                            (repeatedly #(ReentrantLock.))))]
    (reify
      SafeArray
      (count [_] (clj/count a))
      (seq [_] (clj/seq a))
      (aget [_ i]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aget a i)
            (finally (.unlock lk)))))
      (aset [this i f]
        (let [lk (clj/aget L (lock-i (inc i) Lsz))]
          (.lock lk)
          (try
            (clj/aset a
                      i
                      (f (aget this i)))
            (finally (.unlock lk))))))))
