(ns joy.ch11
  (:require (clojure [xml :as xml]))
  (:require (clojure [zip :as zip]))
  (:require [joy.ch10 :as ten])
  (:import (java.util.regex Pattern)))

(time (let [x (future (do (Thread/sleep 5000)
                          (+ 41 1)))]
        [@x @x]))

(defn feed->zipper [uri-str]
  (->> (xml/parse uri-str)
       zip/xml-zip))

(defn normalize [feed]
  (if (= :feed (:tag (first feed)))
    feed
    (zip/down feed)))

(defn feed-children [uri-str]
  (->> uri-str
       feed->zipper
       normalize
       zip/children
       (filter (comp #{:item :entry} :tag))))

(defn title [entry]
  (some->> entry
           :content
           (some #(when (= :title (:tag %)) %))
           :content
           first))

(defn count-text-task [extractor txt feed]
  (let [items (feed-children feed)
        re (Pattern/compile (str "(?i)" txt))]
    (->> items
         (map extractor)
         (mapcat #(re-seq re %))
         count)))

(count-text-task
 title
 "Erlang"
 "http://feeds.feedburner.com/ElixirLang")

(def feeds #{"http://feeds.feedburner.com/ElixirLang"
            "http://blog.fogus.me/feed/"})

(let [results (for [feed feeds]
                (future
                  (count-text-task title "Elixir" feed)))]
  (reduce + (map deref results)))

(defmacro as-futures [[a args] & body]
  (let [parts (partition-by #{'=>} body)
        [acts _ [res]] (partition-by #{:as} (first parts))
        [_ _ task] parts]
    `(let [~res (for [~a ~args] (future ~@acts))]
       ~@task)))

(defn occurrences [extractor tag & feeds]
  (as-futures [feed feeds]
              (count-text-task extractor tag feed)
              :as results
              =>
              (reduce + (map deref results))))

#_(occurrences title "released"
            "http://blog.fogus.me/feed/"
            "http://feeds.feedburner.com/ElixirLang"
            "http://www.ruby-lang.org/en/feeds/news.rss")

;; 11.2 Promises
(def x (promise))
(def y (promise))
(def z (promise))

(ten/dothreads! #(deliver z (+ @x @y)))

(ten/dothreads!
 #(do (Thread/sleep 2000) (deliver x 52)))

(ten/dothreads!
 #(do (Thread/sleep 4000) (deliver y 86)))

(time @z)

(defmacro with-promises [[n tasks _ as] & body]
  (when as
    `(let [tasks# ~tasks
           n# (count tasks#)
           promises# (take n# (repeatedly promise))]
       (dotimes [i# n#]
         (ten/dothreads!
          (fn []
            (deliver (nth promises# i#)
                     ((nth tasks# i#))))))
       (let [~n tasks#
             ~as promises#]
         ~@body))))

(defrecord TestRun [run passed failed])

(defn pass [] true)
(defn fail [] false)

(defn run-tests [& all-tests]
  (with-promises
    [tests all-tests :as results]
    (into (TestRun. 0 0 0)
          (reduce #(merge-with + %1 %2) {}
                  (for [r results]
                    (if @r
                      {:run 1 :passed 1}
                      {:run 1 :failed 1}))))))

(run-tests pass fail fail fail pass)

;; 11.2.2 Callback API to blocking API

(defn tweet-items [k feed]
  (k
   (for [item (filter (comp #{:entry :item} :tag)
                      (feed-children feed))]
     (-> item :content first :content))))

(tweet-items
 count
 "http://api.twitter.com/1/statuses/user_timeline.rss?user_id=46130870")

(let [p (promise)]
  (tweet-items #(deliver p (count %))
               "https://api.twitter.com/1.1/statuses/user_timeline.rss?user_id=46130870")
  @p)

(defn cps->fn [f k]
  (fn [& args]
    (let [p (promise)]
      (apply f (fn [x] (deliver p (k x))) args)
      @p)))

(def count-items (cps->fn tweet-items count))

(count-items "https://api.twitter.com/1.1/statuses/user_timeline.rss?user_id=46130870")

(def kant (promise))
(def hume (promise))

(ten/dothreads!
 #(do (println "Kant has" @kant) (deliver hume :thinking)))

(ten/dothreads!
 #(do (println "hume is" @hume) (deliver kant :fork)))
