(ns whelmed.songs.SKA
  (:use
        [leipzig.melody]
        [leipzig.live]
        [leipzig.scale]
        [leipzig.chord]
        [whelmed.instrument]
        [whelmed.contrib.harpsichord]
        [whelmed.melody]
        [overtone.live :only [recording-start recording-stop ctl at midi->hz now]]))

; Introduction
(def once
  (->> (phrase (cycle [5/3 1 1/3 1])
               [0 0 2 4 5 5 4 2])
       (having :open (cycle [false false false true]))
       (all :part ::bass)
       (where :pitch (comp lower lower))))

(def bass
  (->> once (times 2)))

; Groove
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
    (all :part ::melody)))

(def rhythm-section
  (let [home seventh
        away (-> ninth (root 6) (inversion 1))]
    (->>
    (phrase (repeat 1) (interleave (repeat nil) [home home away away]))
    (times 2)
    (all :duration 0.05)
    (all :part ::rhythm))))

(def suns-on-the-rise 
  (let [[home up down] [seventh {:i 1.5 :iii 3 :v 5.5} seventh]]
    (->>
      (phrase (repeat 1) (interleave (repeat nil) (mapcat repeat [2 2 16] [up down home])))
      (all :duration 0.05)
      (all :part ::rhythm))))

(def oooh
  (->>
    (phrase
      [3 2/3 1/3 3 2/3 1/3 3]
      [3 4 3 2 0 -1 0]) 
    (where :pitch raise)
    (all :part ::melody)))

(def sunrise 
  (let [[home up] [seventh {:i 1.5 :iii 3 :v 5.5}]]
    (->>
      (phrase (repeat 8 4) (cons up (repeat home)))
      (all :part ::sunrise))))

(def groove
  (->>
    (with bass wish-you-were-here-again rhythm-section)
    (times 2)
    (then (with suns-on-the-rise sunrise oooh (after 8 (times 2 bass))))))

; Plateau
(def and-if-you-lived-here
  (->>
    (phrase
    (repeat 4)
    [triad
     (-> triad (root -3))
     (-> triad (root 1) (augment :iii 1/2))
     (-> triad (root -2) (augment :iii 1/2))])
    (all :part ::rhythm)))

(def youd-be-home-by-now
  (->>
    (phrase
      [1/3 1/3 1/3 2/3 1 1 1 1 1 10/3 2/3 4]
      [-5 -3 0 2 2 1 -1 -2 -1 -2 -3 -2])
    (after 3/3)
    (all :part ::melody)))

(def youd-be-home-right-now
  (->>
    youd-be-home-by-now
    (drop-last 2)
    (then
      (phrase [2/3 4] [1 0.5]))
    (all :part ::melody)))

(def mid-section
  (->> and-if-you-lived-here 
       (with youd-be-home-by-now) 
       (then (->>
               and-if-you-lived-here
               (with youd-be-home-right-now)))
       (times 2)))
 
; Return
(def fallbass
  (->> (take 4 bass)
       (then
         (phrase [4] [(lower -3.5)]))
       (all :part ::bass)))

(def fallchords
  (->> (take 6 rhythm-section)
       (then
         (->>
           (phrase [2] [(-> triad (root 3.5) (augment :iii 1/2))])
           (after 2)))
       (all :part ::rhythm)))

(def falla
  (all :part ::melody
       (phrase
         [1/3 1/3 1/3 2/3 1/3]
         [0.5 3.5 6 5 3.5])))

(def fallb
  (all :part ::melody
       (phrase
         [1/3 1/3 1/3 1/3 1/3 1/3]
         [6 5 3.5 6 5 3.5])))

(def rise
  (->> fallbass
       (with fallchords (after 6 falla))
       (then
         (->> fallbass
              (with fallchords)
              (with (after 6 fallb))))
       (then
         (->> fallbass
              (with fallchords)
              (with (after 6 falla))))
       (then (take 5 fallbass))
       (then (->> (after -4 (phrase (repeat 2/3) [3.5 3 2.5 2 1 0.5])) (all :part ::bass)))))

; Structure
(def ska
  (->>
    (->> bass
         (then groove)
         (where :pitch (comp F minor)))
    (then (->> mid-section
               (where :pitch (comp low B major))))
    (then (->> rise
               (then groove)
               (where :pitch (comp F minor))))
    (then (->> mid-section
               (where :pitch (comp low B major))))
    (then (->> rise
               (then (->> groove (in-time (partial * 3/2))))
               (then (->> groove (phrase [4] [-14]) (all :part ::bass)))
               (where :pitch (comp F minor))))
    (in-time (bpm 180))))

(defmethod play-note ::bass [{midi :pitch seconds :duration open? :open}]
  (some-> midi midi->hz
          (corgan :vol (if open? 0.8 0.6) :under-attack 0.1 :attack 0.001 :dur (* 2/3 seconds) :wet 0.1 :room 0.4 :pan 1/3 :vibrato (if open? 9 1) :limit (if open? 4000 1500))))

(defmethod play-note ::rhythm [{midi :pitch, s :duration}]
  (some-> midi midi->hz (organ s :vol 4 :limit 2000 :attack 0 :pan -1/3 :room 0.4 :wet 0.9)))   

(defmethod play-note ::melody [{midi :pitch s :duration}]
  (some-> midi midi->hz (sawish :volume 1.2 :duration 0.3 :vibrato 1 :wet 0.5 :room 0.01 :limit 3000 :depth 1))
  (some-> midi midi->hz (harpsichord :volume 0.9 :duration s :vibrato 1 :wet 0.3 :room 0.4 :limit 2000 :depth 1)))

(defmethod play-note ::sunrise [{midi :pitch s :duration}]
  (some-> midi midi->hz (brassy :walk 1/2 :pan 1/2 :attack 0.5 :vol 0.3 :dur s :wet 0.4 :room 0.9 :limit 5000 :p 2))
  (some-> midi midi->hz (brassy :walk 1/2 :pan 1/2 :attack 0.1 :vol 0.7 :dur s :wet 0.6 :room 0.4 :limit 4000)))


(comment
  (recording-start "ska.wav")
  (play ska)
  (recording-stop)
  (jam (var ska))
)
