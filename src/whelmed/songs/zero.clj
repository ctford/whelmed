(ns whelmed.songs.zero
  (:require [leipzig.live :as live]))

(defmethod live/play-note :piano [_])

(def zero
  "A cover version of John Cage's 273, but accurate to two more decimal places."
  [{:time 0 :duration 273.15 :part :piano}])

(comment
  (->> zero live/play)
)
