(ns joy.ch10
  (:import java.util.concurrent.Executors))

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
