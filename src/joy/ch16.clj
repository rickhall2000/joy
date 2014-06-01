(ns joy.ch16
  (:require [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.core.logic :as logic]))

(def b1 '[3 - - - - 5 - 1 -
         - 7 - - - 6 - 3 -
         1 - - - 9 - - - -
         7 - 8 - - - - 9 -
         9 - - 4 - 8 - - 2
         - 6 - - - - 5 - 1
         - - - - 4 - - - 6
         - 4 - 7 - - - 2 -
         - 2 - 6 - - - - 3])

(defn prep [board]
  (map #(partition 3 %)
       (partition 9 board)))

(defn print-board [board]
  (let [row-sep (apply str (repeat 43 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (print "|  ")
      (doseq [subrow (nth board row)]
        (doseq [cell (butlast subrow)]
          (print (str cell "   ")))
        (print (str (last subrow) "  |  ")))
      (println)
      (when (zero? (mod (inc row) 3))
        (println row-sep)))))

(-> b1 prep print-board)

(defn rows [board sz]
  (partition sz board))

(defn row-for [board index sz]
  (nth (rows board sz) (/ index 9)))

(row-for b1 1 9)

(defn column-for [board index sz]
  (let [col (mod index sz)]
    (map #(nth % col)
         (rows board sz))))

(column-for b1 2 9)

(defn subgrid-for [board i]
  (let [rows (rows board 9)
        sgcol (/ (mod i 9) 3)
        sgrow (/ (/ i 9) 3)
        grp-col (column-for
                 (mapcat #(partition 3 %) rows) sgcol 3)
        grp (take 3 (drop (* 3 (int sgrow)) grp-col))]
    (flatten grp)))

(subgrid-for b1 0)

(defn numbers-present-for [board i]
  (set
   (concat (row-for board i 9)
           (column-for board i 9)
           (subgrid-for board i))))

(numbers-present-for b1 1)

(set/difference #{1 2 3 4 5 6 7 8 9}
                (numbers-present-for b1 1))

(defn possible-placements [board index]
  (set/difference #{1 2 3 4 5 6 7 8 9}
                  (numbers-present-for board index)))

#_(defn pos [pred coll]
  (for [[i v] (index coll)
        :when (pred v)] i))

#_(defn solve [board]
  (if-let [[i & _]
           (and (some '#{-} board)
                (pos '#{-} board))]
    (flatten (map #(solve (assoc board i %))
                  (possible-placements board i)))
    board))

#_(-> b1
    solve
    prep
    print-board)

;; Thinking Data Via Unification

(defn lvar? [x]
  (boolean
   (when (symbol? x)
     (re-matches #"^\?.*" (name x)))))

(lvar? '?x)
(lvar? 'a)
(lvar? 1)

(defn satisfy1
  [l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
     (= L R) knowledge
     (lvar? L) (assoc knowledge L R)
     (lvar? R) (assoc knowledge R L)
     :default nil)))

(satisfy1 '?something 2 {})

(satisfy1 '?x '?y {})

(->> {}
     (satisfy1 '?x '?y)
     (satisfy1 '?x 1))

(defn satisfy [ l r knowledge]
  (let [L (get knowledge l l)
        R (get knowledge r r)]
    (cond
     (not knowledge) nil
     (= L R) knowledge
     (lvar? L) (assoc knowledge L R)
     (lvar? R) (assoc knowledge R L)
     (every? seq? [L R])
     (satisfy (rest L)
              (rest R)
              (satisfy (first L)
                       (first R)
                       knowledge))
     :default nil)))

(satisfy '(1 2 3) '(1 ?something 3) {})

(satisfy '(((?something))) '(((2))) {})

(satisfy '(?x 2 3 (4 5 ?z))
         '(1 2 ?y (4 5 6))
         {})

(satisfy '(?x 10000 3) '(1 2 ?y) {})

(defn subst [term binds]
  (walk/prewalk
   (fn [expr]
     (if (lvar? expr)
       (or (binds expr) expr)
       expr))
   term))

(subst '(1 ?x 3) '{?x 2})

(subst '(1 ?x 3) '{})

(def page '[:html
            [:head [:title ?title]]
            [:body [:h1 ?title]]])

(subst page '{?title "Hi!"})

(defn meld [term1 term2]
  (->> {}
       (satisfy term1 term2)
       (subst term1)))

(meld '(1 ?x 3) '(1 2 ?y))

(meld '(1 ?x) '(?y (?y 2)))

;; 16.3 Core.Logic
(logic/run* [answer]
            (logic/== answer 5))

(logic/run* [val1 val2]
            (logic/== {:a val1 :b 2}
                      {:a 1, :b val2}))

(logic/run* [x y]
            (logic/== x y))

(logic/run* [q]
            (logic/== q 1)
            (logic/== q 2))

(logic/run* [george]
            (logic/conde
             [(logic/== george :born)]
             [(logic/== george :unborn)]))

#_(logic/defrel orbits oribtal body)

#_(logic/fact orbits :mercury :sun)

#_(logic/run* [q]
            (logic/fresh [orbital body]
                         (orbits orbital body)
                         (logic/== q orbital)))

(comment
  (logic/defrel stars star)
  (logic/fact stars :sun)
  (defn planeto [body]
    (logic/fresh [star]
                 (stars star)
                 (orbits body star)))

  (logic/run* [q]
              (planeto :earth)
              (logic/== q true))

  (logic/run* [q]
              (planeto :sun)
              (logic/== q true))

  (logic/run* [q]
              (logic/fresh [orbital]
                           (planeto orbital)
                           logic/== q orbital))

  (logic/fact stars :alpha-centauri)

  (logic/fact orbits :Bb :alpha-centauri)

  (logic/run* [q]
              (planeto :Bb))

  (defn satelliteo [body]
    (logic/fresh [p]
                 (oribts body p)
                 (planeto p)))

  (logic/run* [q]
              (satelliteo :sun))

  (logic/run [q]
             (satelliteo :earth))

  (logic/fact orbits :moon :earth)

  (logic/run* [q]
              (satelliteo :moon))

  (logic/fact orbits :phobos :mars)

  (logic/fact obrits :deimos :mars)

  )

;; 16.4 Constraints

(logic/run* [q]
            (logic/fresh [x y]
                         (logic/== [:pizza "Java"] [x y])
                         (logic/== q [x y])))

(logic/run* [q]
            (logic/fresh [x y]
                         (logic/== q [x y])
                         (logic/!= y "Java")))

(logic/run* [q]
            (logic/fresh [x y]
                         (logic/== [:pizza "Java"] [x y])
                         (logic/== q [x y])
                         (logic/!= y "Java")))

(logic/run* [q]
            (logic/fresh [x y]
                         (logic/== [:pizza "Scala"] [x y])
                         (logic/== q [x y])
                         (logic/!= y "Java")))

(logic/run* [q]
            (logic/fresh [n]
                         (logic/== q n)))

(require '[clojure.core.logic.fd :as fd])

(logic/run* [q]
            (logic/fresh [n]
                         (fd/in n (fd/domain 0 1))
                         (logic/== q n)))

(logic/run* [q]
            (let [coin (fd/domain 0 1)]
              (logic/fresh [heads tails]
                           (fd/in heads 0 coin)
                           (fd/in tails 1 coin)
                           (logic/== q [heads tails]))))

(defn rowify [board]
  (->> board
       (partition 9)
       (map vec)
       vec))

(rowify b1)

(defn colify [rows]
  (apply map vector rows))

(colify (rowify b1))

(defn subgrid [rows]
  (partition 9
             (for [row (range 0 9 3)
                   col (range 0 9 3)
                   x (range row (+ row 3))
                   y (range row (+ col 3))]
               (get-in rows [x y]))))

(subgrid (rowify b1))

(def logic-board #(repeatedly 81 logic/lvar))

(defn init [[lv & lvs] [cell & cells]]
  (if lv
    (logic/fresh []
                 (if (= '- cell)
                   logic/succeed
                   (logic/== lv cell))
                 (init lvs cells))
    logic/succeed))

(defn solve-logically [board]
  (let [legal-nums (fd/interval 1 9)
        lvars (logic-board)
        rows (rowify lvars)
        cols (colify rows)
        grids (subgrid rows)]
    (logic/run 1 [q]
               (init lvars board)
               (logic/everyg #(fd/in % legal-nums) lvars)
               (logic/everyg fd/distinct rows)
               (logic/everyg fd/distinct cols)
               (logic/everyg fd/distinct grids)
               (logic/== q lvars))))

(-> b1
    solve-logically
    first
    prep
    print-board)
