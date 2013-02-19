(ns whelmed.songs.in-the-mood
  (:use
    [leipzig.melody]
    [whelmed.melody]
    [leipzig.scale] 
    [leipzig.canon] 
    [overtone.inst.sampled-piano]
    [whelmed.instrument]) 
  (:require
    [overtone.live :as overtone]
    [overtone.inst.drum :as drums]
    [overtone.synth.stringed :as strings])) 

(defn in-time [speed notes]
  (->> notes
    (where :time speed)
    (where :duration speed)))

(->> riff (take 15) play)

(def riff
  (let [melody (->>
                 (phrase (cycle [2/6 1/6]) (cycle [2 4 7]))
                 (take 11)
                 (then (phrase [7/6] [7])))]
  (->>
    (times 4 melody)
    (then (->> melody (where :pitch inc)))
    (then melody)
    (with (->>
            (phrase (cycle [7/2 1/2]) 
                    [0 0 0 0 3 3 0 0 -3 -3 0 0])
            (where :pitch low)
            (canon (comp (interval 2) (partial canon (interval 2))))))
    (in-time (bpm 100))
    (where :part (is ::melody))
    (where :pitch (comp G major)))))

(defmethod play-note ::melody [note] (pick 0.99 0.3 note))
(defmethod play-note ::melody [{midi :pitch}] (sampled-piano midi))

;(def riff nil) 
;(jam riff)

