(ns whelmed.melody
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.chord]
    [leipzig.scale]))

(defn between? [from to]
  (fn [note] 
    (and (>= (:time note) from) (< (:time note) to))))

(defn augment [chord k n] (update-in chord [k] (from n)))

(def in-time tempo)

(defn accelerando
  "Linearly interpolated change between from and to."
  [from to by]
  (fn rate [t]
    (cond
      (>= from t) t
      (>= to t) (let [duration (- to from)
                      position (- t from)
                      completion (/ position duration)
                      extent (- by 1)
                      base t
                      extra (* position 1/2 completion extent)]
                  (+ base extra)) 
      :otherwise (+ (rate to) (* by (- t to))))))
