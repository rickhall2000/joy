(ns joy.ch14
  (:require [clojure.data :as d]
            [clojure.edn :as edn]
            [clojure.set :as sql]))

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

;;#unit/legth [1 :km]

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

#_(edn/read-string "#ch14/time [1 :min 30 :sec]")

(def T ('ch14/time #'joy.ch14/time-reader))

#_(edn/read-string {:readers T} "#ch14/time [1 :min 30 :sec]")

(edn/read-string {:readers T, :default vector} "#what/the :huh?")

;; 14.3
(defn valid? [event]
  (boolean (:result event)))

(valid? {})
(valid? {:result 42})

(defn effect [{:keys [ab h] :or {ab 0, h 0}} event]
  (let [ab (inc ab)
        h (if (= :hit (:result event)) (inc h) h)
        avg (double (/ h ab))]
    {:ab ab :h h :avg avg}))

(effect {} {:result :hit})

(effect {:ab 599 :h 180}
        {:result :out})

(defn apply-effect [state event]
  (if (valid? event) (effect state event) state))

(apply-effect {:ab 600 :h 180}
              {:result :hit})

(def effect-all #(reduce apply-effect %1 %2))

(effect-all {:ab 0 :h 0}
            [{:result :hit}
             {:result :out}
             {:result :hit}
             {:result :out}])

(def events
  (repeatedly 100
              (fn [] (rand-map 1
                              #(-> :result)
                              #(if (<  (rand-int 10) 3)
                                 :hit :out)))))

(effect-all {} events)
(effect-all {} (take 50 events))

(def fx-timeline #(reductions apply-effect %1 %2))

(fx-timeline {} (take 3 events))

(def PLAYERS #{{:player "Nick" :ability 32} {:player "Matt" :ability 26} {:player "Ryan" :ability 19}})

(defn lookup [db name]
  (first (sql/select #(= name (:player %)) db)))

(lookup PLAYERS "Nick")

(defn update-stats [db event]
  (let [player (lookup db (:player event))
        less-db (sql/difference db #{player})]
    (conj less-db (merge player (effect player event)))))

(update-stats PLAYERS {:player "Nick" :result :hit})

(defn commit-event [db event]
  (dosync (alter db update-stats event)))

(commit-event (ref PLAYERS) {:player "Nick" :result :hit})

(defn rand-event [{ability :ability}]
  (let [able (numerator ability)
        max (denominator ability)]
    (rand-map 1 #(-> :result) #(if
                                  (< (rand-int max) able)
                                :hit :out))))

(defn rand-events [total player]
  (take total (repeatedly #(assoc
                               (rand-event player) :player
                               (:player player)))))

(rand-events 3 {:player "Nick" :ability 32/100})

(def agent-for-player
  (memoize (fn [player-name]
             (-> (agent [])
                 (set-error-handler! #(println "ERROR: " %1 %2))
                 (set-error-mode! :fail)))))

(defn feed [db event]
  (let [a (agent-for-player (:player event))]
    (send a
          (fn [state]
            (commit-event db event
                          (conj state event))))))

(defn feed-all [db events]
  (doseq [event events]
    (feed db event)) db)

#_(let [db (ref PLAYERS)]
  (feed-all db
            (rand-events 100
                         {:player "Nick" :ability 32/100}))
  db)

#_(count @(agent-for-player "Nick"))

(defn simulate [total players]
  (let [events
        (apply interleave
               (for [player players]
                 (rand-events total player)))
        results
        (feed-all (ref players) events)]
    (apply await
           (map #(agent-for-player
                  (:player %)) players))
    @results))

;; 14.4 Code as Data as Code

(defn meters->feet [m] (* m 3.28084))
(defn meters->miles [m] (* m 0.000621))

(meters->feet 1609.344)
(meters->miles 1609.344)

(comment
  (our base unit of distance is the :meter
       [There are 1000 :meters in a :kilometer]
       [There are 100 :centemeters in a :meter]
       ...))

(comment
  (define unit of distance
    {:m 1
     :km 1000
     :cm 1/10
     :mm [1/10 of a :cm]
     :ft 0.3048
     :mile [is 5280 :ft]}))

(defn relative-units [context unit]
  (if-let [spec (get context unit)]
    (if (vector? spec)
      (convert context spec)
      spec)
    (throw
     (RuntimeException. (str "undefined nit " unit)))))

(relative-units {:m 1 :cm 100 :mm [10 :cm]} :m)

(relative-units {:m 1 :cm 1/100 :mm [1/10 :cm]} :mm)

(relative-units {:m 1 :cm 100 :mm [10 :cm]} :what)

(defmacro defunits-of [name base-unit & conversions]
  (let [magnitude (gensym)
        unit (gensym)
        units-map (into `{~base-unit 1}
                        (map vec (partition 2 conversions)))]
    `(defmacro ~(symbol (str "unit-of-" name))
       [~magnitude ~unit]
       `(* ~~magnitude
           ~(case ~unit
              ~@(mapcat
                 (fn [[u# & r#]]
                   `[~u# ~(relative-units
                           units-map
                           u#)])
                 units-map))))))

(defunits-of distance :m
  :km 1000
  :cm 1/100
  :mm [1/10 :cm]
  :ft 0.3048
  :mile [5280 :ft])

(unit-of-distance 1 :m)
(unit-of-distance 1 :mm)
(unit-of-distance 441 :ft)

(macroexpand-1 '(defunits-of distance :m
  :km 1000
  :cm 1/100
  :mm [1/10 :cm]
  :ft 0.3048
  :mile [5280 :ft]))

(macroexpand-1 '(unit-of-distance 1 :cm))
