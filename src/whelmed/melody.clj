(ns whelmed.melody)

(defn cut [start end notes] (->> notes (take end) (drop start)))
(defn except [start end notes]
      (concat
                  (take start notes)
                  (drop end notes)))
