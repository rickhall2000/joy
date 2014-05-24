(ns joy.macro-tunes)

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
              {:delay 0 :duration 0}
              notes))

(defn notes [tone-pairs]
  (let [bpm 360
        bps (/ bpm 60)]
    (->> tone-pairs
         (map pair-to-note)
         consecutive-notes
         (map #(update-in % [:delay] (comp double /) bps))
         (map #(update-in % [:duration] (comp double /) bps)))))

(defn magical-theme
  []
  notes
  (concat
   [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
   [[21 6] [18 6] [16 3] [19 1] [18 2] [14 4] [17 2] [11 10]]
   [[11 2] [16 3] [19 1] [18 2] [16 4] [23 2]]
   [[26 4] [25 2] [24 4] [20 2] [24 3] [23 1] [22 2] [10 4]       [19 2] [16 10]]))

(defmacro magical-theme-macro []
  (vec (magical-theme)))
