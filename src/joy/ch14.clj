(ns joy.ch14
  (:require [clojure.data :as d]
            [clojure.edn :as edn]))

(rand-int 1024)
(+ (rand-int 100) (rand-int 100))

(def ascii (map char (range 65 (+ 65 26))))
ascii

(defn rand-str [sz alphabet]
  (apply str (repeatedly sz #(rand-nth alphabet))))

(rand-str 10 ascii)

(def rand-sym #(symbol (rand-str %1 %2)))
(def rand-key #(keyword (rand-str %1 %2)))

(rand-key 10 ascii)
(rand-sym 10 ascii)

(defn rand-vec [& generators]
  (into [] (map #(%) generators)))
(rand-vec #(rand-sym 5 ascii)
          #(rand-key 10 ascii)
          #(rand-int 1024))

(defn rand-map [sz kgen vgen]
  (into {}
       (repeatedly sz #(rand-vec kgen vgen))))

(rand-map 3 #(rand-key 5 ascii) #(rand-int 100))

(assert (= [1 2 3] (conj [1 2] 3)))

(d/diff [1 2 3] [1 2 4])

#_(defspec slope-rules
  (fn [p1 p2] (slope :p1 p1 :p2 p2))
  [[^long ^long] [^long ^long]]
  (assert (float? %)))

(defn filter-rising [segments]
  (clojure.set/select
   (fn [{:keys [p1 p2]}]
     (> 0
        (/ (- (p2 0) (p1 0))
           (- (p2 1) (p1 1)))))
   segments))

(filter-rising #{{:p1 [0 0] :p2 [1 1]}
                 {:p1 [4 15] :p2 [3 21]}})

#inst "1969-08-18"

(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(def distance-reader
  (partial convert
           {:m 1
            :km 1000
            :cm 1/100
            :mm [1/10 :cm]}))

#unit/legth [1 :km]

(def time-reader
  (partial convert
           {:sec 1
            :min 60
            :hr [60 :min]
            :day [24 :hr]}))

(binding [*data-readers* {'ch14/time #'joy.ch14/time-reader}]
  (read-string "#ch14/time [1 :min 30 :sec]"))

(binding [*default-data-reader-fn* #(-> {:tag %1 :payload %2})]
  (read-string "#nope [:doesnt-exist]"))

#_(edn/read-string "#uuid ")

(edn/read-string "42")

(edn/read-string "#ch14/time [1 :min 30 :sec]")

(def T ('ch14/time #'joy.ch14/time-reader))

(edn/read-string {:readers T} "#ch14/time [1 :min 30 :sec]")

(edn/read-string {:readers T, :default vector} "#what/the :huh?")
