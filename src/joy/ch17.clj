(ns joy.ch17
  (:require [clojure.set :as ra]
            #_[joy.ch11 :as joy]
            [clojure.test :refer [deftest testing is]])
  (:use [clojure.string :as str :only []]))

(def artists
  #{{:artist "Burial" :genre-id 1}
    {:artist "Magma" :genre-id 2}
    {:artist "Can" :genre-id 3}
    {:artist "Faust" :genre-id 3}
    {:artist "Ikonika" :genre-id 1}
    {:artist "Grouper"}})

(def genres
  #{{:genre-id 1 :genre-name "Dubstep"}
    {:genre-id 2 :genre-name "Zehul"}
    {:genre-id 3 :genre-name "Prog"}
    {:genre-id 4 :genre-name "Drone"}})

(def ALL identity)

(ra/select ALL genres)

(ra/select (fn [m] (#{1 3} (:genre-id m))) genres)

(defn ids [& ids]
  (fn [m] ((set ids) (:genre-id m))))

(ra/select (ids 1 3) genres)

(take 2 (ra/select ALL (ra/join artists genres)))

#_(defn fantasy-query [max]
  (SELECT [a b c]
          (FROM X
                (LEFT-JOIN :ON (= X.a Y.b)))
          (WHERE (< a 5) AND (< b max))))

(defn shuffle-expr [expr]
  (if (coll? expr)
    (if (= (first expr) `unquote)
      "?"
      (let [[op & args] expr]
        (str "("
             (str/join (str " " op " ")
                       (map shuffle-expr args)) ")")))
    expr))

(shuffle-expr 42)

(shuffle-expr `(unquote max))

(read-string "~max")

(shuffle-expr '(= X.a Y.b))

(shuffle-expr '(AND (< a 5) (< b ~max)))

(defn process-where-clause [processor expr]
  (str " WHERE " (processor expr)))

(process-where-clause shuffle-expr '(AND (< a 5) (< b ?)))

(defn process-left-join-clause [processor table _ expr]
  (str " LEFT JOIN " table
       " ON " (processor expr)))

(let [LEFT-JOIN (partial process-left-join-clause shuffle-expr)]
  (LEFT-JOIN 'Y :ON '(= X.a Y.b)))

(defn process-from-clause [processor table & joins]
  (apply str " FROM " table
         (map processor joins)))

(process-from-clause shuffle-expr 'X
                     (process-left-join-clause shuffle-expr 'Y
                                               :ON '(= X.a Y.b)))

(defn process-select-clause [processor fields & clauses]
  (apply str "SELECT " (str/join ", " fields)
         (map processor clauses)))

(process-select-clause shuffle-expr
                       '[a b c]
                       (process-from-clause shuffle-expr 'X
                                            (process-left-join-clause shuffle-expr 'Y
                                                                      :on '(= X.a Y.b)))
                       (process-where-clause shuffle-expr '(AND (< a 5) (< b ~max))))

(declare apply-syntax)

(def ^:dynamic *clause-map*
  {'SELECT (partial process-select-clause apply-syntax)
   'FROM (partial process-from-clause apply-syntax)
   'LEFT-JOIN (partial process-left-join-clause shuffle-expr)
   'WHERE (partial process-where-clause shuffle-expr)})

(defn apply-syntax [[op & args]]
  (apply (get *clause-map* op) args))

(defmacro SELECT [& args]
  {:query (apply-syntax
           (cons 'SELECT args))
   :bindings (vec (for [n (tree-seq coll? seq args)
                        :when (and (coll? n)
                                   (= (first n) `unquote))]
                    (second n)))})

(defn example-query [max]
  (SELECT [a b c]
          (FROM X
                (LEFT-JOIN Y :ON (= X.a Y.b)))
          (WHERE (AND (< a 5) (< b ~max)))))

(example-query 9)

;; 17.2 Testing
(comment
  (def stubbed-feed-children
    (constantly [{:content [{:tage :title
                             :content ["Stub"]}]}]))

  (defn count-feed-entries [url]
    (count (joy/feed-children url)))

  (count-feed-entries "http://blog.fogus.me/feed/")

  (with-redefs [joy/feed-children stubbed-feed-children]
    (count-feed-entries "dummy url"))

  #_(with-redefs [joy/feed-children stubbed-feed-children]
      (joy/occurrences joy/title "Stub" "a" "b" "c"))

  (deftest feed-tests
    (with-redefs [joy/feed-children stubbed-feed-children]
      (testing "Child Counting"
        (is (= 1000 (count-feed-entries "Dummy URL"))))
      (testing "Occurrence Counting"
        (is (= 0 (joy/count-text-task
                  joy/title
                  "ZOMG"
                  "Dummy URL"))))))

  (clojure.test/run-tests))

#_(def sqr (partial
          (contract sqr-contract
                    [n]
                    (require (number? n))
                    (ensure (pos? %)))
          #(* % %)))

#_(doseq [n (range (Short/MIN_VALUE Short/MAX_VALUE))]
  (try
    (sqr n)
    (catch AssertionError e
      (println "Error on input" n)
      (throw e))))

;; 17.3 Design patterns

(defmacro defformula [nm bindings & formula]
  `(let ~bindings
     (let [formula# (agent ~@formula)
           update-fn# (fn [key# ref# o# n#]
                        (send formula# (fn [_#] ~@formula)))]
       (doseq [r# ~(vec (map bindings
                             (range 0 (count bindings) 2)))]
         (add-watch r# :update-forumla update-fn#))
       (def ~nm formula#))))

(def h (ref 25))
(def ab (ref 100))

(defformula avg
  [at-bats ab hits h]
  (float (/ @hits @at-bats)))

@avg

(dosync (ref-set h 33))

(def config
  '{:systems {:pump {:type :feeder :desc "Feeder system"}
              :sim1 {:type :sim, :fidelity :low}
              :sim2 {:type :sim, :fidelity :high, :threads 2}}})

(defn describe-system [name cfg]
  [(:type cfg) (:fidelity cfg)])

(defmulti construct describe-system)

(defmethod construct :default [name cfg]
  {:name name
   :type (:type cfg)})

(defn construct-subsystems [sys-map]
  (for [[name cfg] sys-map]
    (construct name cfg)))

(construct-subsystems (:systems config))

(defmethod construct [:feeder nil]
  [_ cfg]
  (:desc cfg))

(defrecord LowFiSim [name])
(defrecord HiFiSim [name threads])

(defmethod construct [:sim :low]
  [name cfg]
  (->LowFiSim name))

(defmethod construct [:sim :high]
  [name cfg]
  (->HiFiSim name (:threads cfg)))

(construct-subsystems (:systems config))

(def lofi {:type :sim :desc "Lowfi sim" :fidelity :low})
(def hifi {:type :sim :desc "Hifi sim" :fidelity :high :threads 2})

(construct :lofi lofi)

(defprotocol Sys
  (start! [sys])
  (stop! [sys]))

(defprotocol Sim
  (handle [sim msg]))

(defn build-system [name config]
  (let [sys (construct name config)]
    (start! sys)
    sys))

(extend-type LowFiSim
  Sys
  (start! [this]
    (println "Started a lowfi simulator"))
  (stop! [this]
    (println "stopped a lowfi simulator"))
  Sim
  (handle [this msg]
    (* (:weight msg) 3.14)))

(start! (construct :lofi lofi))

(build-system :sim1 lofi)

(handle (build-system :sim1 lofi) {:weight 42})

(extend-type HiFiSim
  Sys
  (start! [this]
    (println "Started a hifi simulator"))
  (stop! [this]
    (println "stopped a hifi simulator"))
  Sim
  (handle [this msg]
    (* (:weight msg) 3.1415926535M)))

(def excellent (promise))

(defn simulate [answer fast slow opts]
  (future (deliver answer (handle slow opts)))
  (handle fast opts))

(simulate excellent
          (build-system :sim1 lofi)
          (build-system :sim2 hifi)
          {:weight 42})

(realized? excellent)
@excellent

(defrecord MockSim [name])
(def starts (atom 0))

(extend-type MockSim
  Sys
  (start! [this]
    (if (= 1 (swap! starts inc))
      (println "Started a mock simulator.")
      (throw (RuntimeException. "called start more than once."))))
  (stop [this] (println "stoped a mock simulator."))

  Sim
  (handle [_ _] 42))

(defmethod construct [:mock nil]
  [nom _]
  (MockSim. nom))

(defn initialize [name cfg]
  (let [lib (:lib cfg)]
    (require lib)
    build-system name cfg))

#_(handle (initialize :mock-sim config) {})

;; 17.4 Error Handling
(defn traverse [node f]
  (when node
    (f node)
    (doseq [child (:content node)]
      (traverse child f))))

(traverse {:tag :flower :attrs {:name "Tanpop"}
           :content []}
          println)

(use '[clojure.xml :as xml])

(def DB
  (-> "<zoo><pongo><animal>orangutan</animal></pongo><panthera><animal>Spot</animal><animal>lion</animal><animal>Lopshire</animal></panthera></zoo>"
      .getBytes
      (java.io.ByteArrayInputStream.)
      xml/parse))

(defn ^:dynamic handle-weird-animal
  [{[name] :content}]
  (throw (Exception. (str name " must be 'dealt with'"))))

(defmulti visit :tag)
(defmethod visit :animal [{[name] :content :as animal}]
  (case name
    "Spot" (handle-weird-animal animal)
    "Lopshire" (handle-weird-animal animal)
    (println name)))

#_(traverse DB visit)

(defmulti handle-weird (fn [{[name] :content}] name))

(defmethod handle-weird "Spot" [_]
  (println "Transporting Spot to the circus."))

(defmethod handle-weird "Lopshire" [_]
  (println "Signing Lopshire to a book deal."))

#_(binding [handle-weird-animal handle-weird]
  (traverse DB visit))

(def _ (future
         (binding [handle-weird-animal #(println (:content %))]
           (traverse DB visit))))

(defn div [n d] (int (/ n d)))
