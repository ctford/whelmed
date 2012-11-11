(ns whelmed.melody
  (:use
    [leipzig.melody]
    [leipzig.scale]))

(defn cut [start end notes] (->> notes (take end) (drop start)))
(defn except [start end notes]
  (concat
    (take start notes)
    (drop end notes)))

(defn but [from to f notes]
  (let [early? #(< (:time %) from)
        late? #(>= (:time %) to)
        apple (->> notes
                (filter #(or (early? %) (late? %)))) 
        core (->> notes
               (filter #(not (early? %))) 
               (filter #(not (late? %))))] 
    (with apple (f core))))

(defn demo
  ([notes] (demo notes major))
  ([scale notes]
    (->> notes
      (where :time (bpm 90))
      (where :duration (bpm 90))
      (where :pitch (comp C scale))
      play)))
