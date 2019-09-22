(ns whelmed.melody
  (:require
    [leipzig.scale :refer [from]]))

(defn between?
  [from to]
  (fn [{:keys [time]}] 
    (and (>= time from) (< time to))))
