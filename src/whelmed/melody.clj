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

(defn accelerando [from to by notes])
