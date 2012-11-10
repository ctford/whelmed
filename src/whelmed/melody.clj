(ns whelmed.melody
  (:use
    [leipzig.melody]
    [leipzig.scale]))

(defn cut [start end notes] (->> notes (take end) (drop start)))
(defn except [start end notes]
  (concat
    (take start notes)
    (drop end notes)))

(defn demo
  ([notes] (demo notes major))
  ([scale notes]
    (->> notes
      (where :time (bpm 90))
      (where :duration (bpm 90))
      (where :pitch (comp C scale))
      play)))
