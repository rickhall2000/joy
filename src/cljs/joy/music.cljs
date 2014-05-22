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
