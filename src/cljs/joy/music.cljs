(ns joy.music)

(defn soft-attack
  [ctx {:keys [volume delay duration]}]
  (let [node (.createGainNode ctx)]
    (doto (.-gain node)
      (.linearRmpToValueAtTime 0 delay)
      (.linearRmpToValueAtTime volume (+ delay 0.05))
      (.linearRmpToValueAtTime 0 (+ delay duration)))
    node))

(defn sine-tone
  [ctx {:keys [cent delay duration]}]
  (let [node (.createOscillator ctx)]
    (set! (-> node .-frequency .-value) 440)
    (set! (-> node .-detune .-value) (- cent 900))
    (.noteOn node delay)
    (.noteOff node (+ delay duration))
    node))

(defn connect-to
  [node1 node2]
  (.connect node1 node2)
  node2)

(defn woo
  [ctx note]
  (let [linger 1.5
        note (update-in note [:duration] * linger)]
    (-> (sine-tone ctx note)
        (connect-to (soft-attack ctx note)))))

(def make-once (memoize (fn [ctor] (new ctor))))

(defn play!
  [note-fn notes]
  (if-let [ctor (or (.-AudioContext js/window)
                    (.-webkitAudioContext js/window))]
    (let [ctx (make-once ctor)
          compressor (.createDynamicsCompressor ctx)]
      (let [now (.-currentTime ctx)]
        (doseq [note notes]
          (->
           (note-fn ctx (update-in note [:delay] + now))
           (connect-to compressor))))
      (connect-to compressor (.-destination ctx)))
    (js/alert "sorry, this browser doesn't support audio context")))

(play! woo [{:cent 1100 :duration 1 :delay 0 :volume 0.6}
            {:cent 1400 :duration 1 :delay 0.2 :volume 0.6}
            {:cent 1800 :duration 1 :delay 0.4 :volume 0.6}])

(defn pair-to-note
  [[tone duration]]
  {:cent (* 100 tone)
   :duration duration
   :volume 0.4})

(defn consecutive-notes
  [notes]
  (reductions (fn [{:keys [delay duration]} note]
                (assoc note
                  :delay (+ delay duration)))
              notes))

(defn notes [tone-pairs]
  (let [bpm 360
        bps (/ bpm 60)]
    (->> tone-pairs
         (map pair-to-note)
         consecutive-notes
         (map #(update-in % [:delay] / bps))
         (map #(update-in % [:duration] / bps)))))

(defn magical-theme
  []
  notes
  (concat
   [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
   [[21 6] [18 6] [16 3] [19 1] [18 2] [14 4] [17 2] [11 10]]
   [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
   [[26 4] [25 2] [24 4] [20 2] [24 3] [23 1] [22 2] [10 4]       [19 2] [16 10]]))

(defn ^:export go []
  (play! woo (magical-theme)))
