(ns whelmed.songs.SKA
  (:use
        [leipzig.melody]
        [leipzig.live]
        [leipzig.scale]
        [leipzig.chord]
        [whelmed.instrument]
        [whelmed.contrib.harpsichord]
        [whelmed.melody]
        [overtone.live :only [ctl at midi->hz now]]))

(def bass
  (->>
      (phrase
        [3/2 1 1/2 1]
        [0   0   2 4])
    (then
      (phrase
        [3/2 1 1/2 1]
        [5   5   4 2]))
    (where :part (is ::bass))
    (where :pitch (comp lower lower))))

(def fallbass
  (->>
      (take 4 bass)
    (then
      (phrase [4] [(lower -3.5)]))))

(def wish-you-were-here-again 
  (->>
      (phrase
        [2/3 1/3 3/3 3/3 2/3 13/3]
        [0 1 0 4 0 0])
    (then
      (phrase
        [2/3 1/3 3/3 3/3 3/3 2/3 1/3 2/3 3/3 4/3]
        [0 1 0 4 0 2 3 2 1 0]))
    (where :pitch raise)
    (where :part (is ::melody))))

(def rhythm-section
  (->>
    (->> (phrase [1] [triad])
      (after 1)
      (times 2))
    (with (->> (phrase [1] [(-> triad (root -2))])
      (after 1)
      (times 2)
      (after 4)))
    (where :part (is ::rhythm))))

(def fallchords
  (->> (take 6 rhythm-section)
       (then
         (->>
           (phrase [2] [(-> triad (root 3.5) (augment :iii 1/2))])
           (after 2)))))

(def falla
  (phrase
    [1/3 1/3 1/3 2/3 1/3]
    [0.5 3.5 6 5 3.5]))

(def fallb
  (phrase
    [1/3 1/3 1/3 1/3 1/3 1/3]
    [6 5 3.5 6 5 3.5]))

(def fallback
  (->> fallbass
    (with fallchords)
    (then
      (->> fallbass
        (with fallchords)
        (with (after 6 fallb))))
    (then
      (->> fallbass
        (with fallchords)
        (with (after 6 falla))))
    (then (take 5 fallbass))
    (then (after -4 (phrase (repeat 2/3) [3.5 3 2.5 2 1 0.5])))
    (where :pitch (comp E minor))))

(def suns-on-the-rise 
  (->>
    (phrase
      [4]
      [(-> triad (root 1) (augment :i 1/2) (augment :v 1/2))])
    (then (phrase [4] [(-> triad (root -2))]))
    (then (phrase [4] [triad]))))

(def oooh
  (->>
    (phrase
      [3 1/3 2/3 3 2/3 1/3 3]
      [3 4 3 2 0 -1 0]) 
    (where :pitch raise)
    (where :part (is ::melody))))

(def and-if-you-lived-here
  (phrase
    (repeat 4)
    [triad
     (-> triad (root -3))
     (-> triad (root 1) (augment :iii 1/2))
     (-> triad (root -2) (augment :iii 1/2))]))

(def youd-be-home-by-now
  (->>
    (phrase
      [1/3 2/3 1 1 1 1 1 10/3 2/3 4]
      [-3 2 2 1 -1 -2 -1 -2 -3 -2])
    (after 5/3)))

(def youd-be-home-right-now
  (->>
    youd-be-home-by-now
    (drop-last 2)
    (then
      (phrase [2/3 4] [1 0.5]))))

(def right-now
  (with and-if-you-lived-here youd-be-home-by-now)) 

(def mid-section
  (->> and-if-you-lived-here 
    (with youd-be-home-by-now) 
    (then (->>
      and-if-you-lived-here
      (with youd-be-home-right-now)))
    (times 2)))

(def first-section
  (->> 
    (->> bass (with rhythm-section) (times 2)
         (with wish-you-were-here-again)
         (times 2))
    (then (with oooh suns-on-the-rise))
    (then (->> bass (times 2) (after -4)))
    (where :pitch (comp E minor))))

(def intro (->> bass (times 2) (where :pitch (comp E minor))))

(def ska
  (->>
    intro
    (then first-section)
    (then intro)
    (then first-section)
    (then (where :pitch (comp low B flat major) mid-section))
    (then fallback)
    (then (in-time #(* 4/3 %) first-section))
    (wherever (comp not :part) :part (is ::default))
    (in-time (bpm 180))))

(defmethod play-note ::bass [{midi :pitch}] (-> midi midi->hz (harpsichord 0.3)))
(defmethod play-note ::rhythm [{midi :pitch, ms :duration}]
  (organ (midi->hz midi) ms))   
(defmethod play-note ::default [note]
  (-> note (assoc :part ::rhythm) play-note))
(defmethod play-note ::melody [note]
  (-> note (assoc :part ::rhythm) play-note))

(comment
  (play ska)
)
