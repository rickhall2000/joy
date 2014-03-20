(ns joy.ch8)

(+ 1 2)

(eval '(list 1 2))

(eval (list (symbol "+") 1 2))


(defn contextual-eval [ctx expr]
  (eval
   `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)]
      ~expr)))

(contextual-eval '{a 1, b 2} '(+ a b))

(contextual-eval '{a 1 b 2} '(let [b 1000] (+ a b)))

(println "hello")

;; 8.2
(defmacro do-until [& clauses]
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(do-until
 (even? 2) (println "even")
 (odd? 3) (println "odd")
 (zero? 1) (println "you never see me")
 :lollipop (println "Truthy Thing"))

(macroexpand-1 '(do-until true (prn 1) false (prn 2)))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless (even? 3) "now we see it...")

(unless (even? 2) "now we dont")

(macroexpand-1 '(unless (even? 3) "now we see it"))

;; 8.3

(defmacro def-watched [name & value]
  `(do
     (def ~name ~@value)
     (add-watch (var ~name)
                :re-bind
                (fn [~'key ~'r old# new#]
                  (println old# " -> " new#)))))

(def-watched x 2)

(def x 0)

(defmacro domain [name & body]
  `{:tag :domain,
   :attrs {:name (str '~name)},
   :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping,
   :attrs {:name (str '~name)},
   :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing,
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c] [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
           (list? a) [:isa (str (second a))]
           (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties :attrs nil
     :content (apply vector (for [p props]
                              {:tag :property
                               :attrs {:name (str (first p))}
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock human")
                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elusive crature"
                     [eats-goats?]))))


(:tag d)

(:tag (first (:content d)))

(use '[clojure.xml :as xml])
(xml/emit d)

;; 8.5
(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(awhen [1 2 3] (it 0))
(awhen nil (println "not going to happen"))
(awhen 1 (awhen 2 [it]))

(defmacro with-resource [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
        (~close-fn ~(binding 0))))))

(declare collect-bodies)

(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)

(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) 'require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) 'ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception.
                                  (str "Unknown tag "
                                       (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))

(times2 9)

(def times3 (partial doubler-contract #(* 3 %)))

(times3 9)

(macroexpand-1 '(contract doubler [x]
                          (require (pos? x)) (ensure (= (* 2 x) %))))

(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))
            [x y]
            (require
             (pos? x)
             (pos? y))
            (ensure
             (= (* 2 (+ x y)) %))))

((partial doubler-contract #(* 2 (+ %1 %2))) 2 3)
((partial doubler-contract #(+ %1 %1 %2 %2)) 2 3)




