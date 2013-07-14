(ns whelmed.songs.chicago
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.canon]
    [whelmed.melody]
    [leipzig.scale] 
    [overtone.inst.sampled-piano]
    [whelmed.instrument]) 
  (:require
    [overtone.live :as overtone]
    [overtone.inst.drum :as drums]
    [overtone.synth.stringed :as strings])) 

(def twelvebar [[0 0] [3 0] [4 0]])
(def other
  (let [bass #(->> (phrase
                     (concat (repeat 7 1) [1/2]) 
                     [0 2 3 3.5 4 6 7 9])
                   (where :pitch (from (first %))) 
                   (canon (comp (interval 7) (simple 1/2)))
                   (then (->>
                           (phrase
                             (concat (repeat 7 1) [1/2])
                             [0 2 3 3.5 4 6 7 9])
                           (with (->> (phrase
                                        (concat (repeat 6 1) (repeat 6 1/4))
                                        [7 9 9.5 10 9.5 9 7 6 4 2 -1 0])
                                      (after 1/2)
                                      ))
                           
                         (where :pitch (from (last %)))) 
                         )
                   (where :pitch (comp lower lower))
                   ) 
        melody []
        ]
    (->> (mapthen bass twelvebar)
         (with melody)
         )))

(def progression [4 0])

(def riff
  (let [bass (->>
               progression
               (mapthen #(->>
                           (phrase [3/2 1/2 2.5 1/2 1/2 1/2 2] 
                                   [0 -1 -3 0 0 -1 -3])
                           (where :pitch (from %))))
               (drop-last 1)
               (then (phrase
                       [1/2 1/2 1/4 1/4 1/4 1/4]
                       [-7 -5 -3 -2 -1 0]))
               (where :pitch (comp lower lower))
               (canon (interval 7)))
        melody (->> (phrase [1/2 1/2 1/4 1/4 1/4 1/4 1/2 3/2] (repeat -1))
                    (times 4))]
    (->>
      bass
      (with melody)
      (then other)
      (where :part (is ::melody))
      (where :pitch (comp E flat aeolian))
      (where :time (bpm 90))
      (where :duration (bpm 90)))))

(defmethod play-note ::melody
  [{midi :pitch, start :time, duration :duration}]
  (let [id (sampled-piano midi)]
    (overtone/at (+ start duration) (overtone/ctl id :gate 0))))

(comment
  (def riff nil) 
  (jam (var riff))
) 
