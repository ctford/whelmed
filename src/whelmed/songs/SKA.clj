(ns whelmed.songs.SKA
  (:use [leipzig.melody]
        [leipzig.live]
        [leipzig.scale]
        [leipzig.chord]
        [whelmed.instrument]
        [whelmed.contrib.harpsichord]
        [whelmed.melody]
        [overtone.inst.drum :as drum]
        [overtone.live :only [recording-start recording-stop ctl at now]])
  (:require [leipzig.temperament :as temperament]))

; Introduction
(def once
  (->> (phrase (cycle [5/3 1 1/3 1])
               [0 0 2 4 5 5 4 2])
       (having :open (cycle [false false false true]))
       (with (->> (phrase (repeat 4) [4 9]) (all :secondary true)))
       (all :part ::bass)
       (where :pitch (comp lower lower))))

(def base
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

(defn suns-on-the-rise [rises]
  (let [[home up down] [seventh (-> triad (root 1) (update-in [:i] (from -1/2))) seventh]
        journey (phrase (repeat 1)
                        (interleave (repeat nil) (mapcat repeat [2 2 4] [up down home])))]
    (->>
      journey
      (times rises)
      (then (phrase (repeat 16 1) (cycle [nil home])))
      (all :duration 0.05)
      (all :part ::rhythm))))

(defn oooh [rises]
  (->>
    (phrase
      [3 2/3 1/3 3 2/3 1/3 8]
      [3 4 3 2 0 -1 0]) 
    (times rises)
    (where :pitch raise)
    (all :part ::melody)))

(defn sunrise [rises] 
  (let [[home up higher] [seventh
                          (-> triad (root 1) (update-in [:i] (from -1/2)))
                          (-> triad (inversion 1) (root 5))]
        journey (phrase (repeat 4) [up higher home home])]
    (->>
      journey
      (times rises)
      (then (phrase (repeat 4 4) (repeat home)))
      (all :part ::sunrise)
      (with (->> (phrase [4 4 4 4] [-6.5 -9 -7 -7])
                 (times rises)
                 (then (phrase (repeat 8 4) (cycle [-7 -7 [-7 -3] [-7 -2]])))
                 (all :part ::harmony))))))

(def basic
  (->>
    (phrase (repeat 1) (cycle [nil -10]))
    (with (phrase (cycle [5/3 1 1/3 1]) (cycle [-14 nil -14 -14])))
    (take-while #(-> % :time (< 8)))
    (all :part ::beat)))

(defn beat [rises]
  (->>
    (times 4 basic)
    (then
      (with
        (times 5 basic)
        (phrase (repeat 40 1) (repeat 14))))
    (all :part ::beat)))

(defn groove [rises]
  (->>
    (with base wish-you-were-here-again rhythm-section)
    (times 2)
    (then (with
            (suns-on-the-rise rises)
            (sunrise rises)
            (oooh rises)
            (after (* rises 16) (times 2 base))))
    (with (beat rises))))

(def extra
  (->> (phrase (repeat 1/3)
               (interleave [0  -3  0  1 2 1 0 -1
                            0  -3  0  1 2 3 4  5
                            6 5 4 6  5 4 3 5 
                            4 3 2 4  3 2 1 0]
                           (repeat -7)
                           (repeat -5)))  
       (then (phrase (repeat 12 1/3) (cycle [0.5 -2 -4])))
       (then (phrase (repeat 12 1/3) (cycle [0 -2 -5])))
       (then (phrase (repeat 24 1/3) (cycle [0 -3 -5])))
       (then (phrase (repeat 12 1/3) (cycle [0.5 -2 -4])))
       (then (phrase (repeat 12 1/3) (cycle [0 -2 -5])))
       (then (phrase (repeat 48 1/3) (cycle [0 -3 -5])))
       (then (phrase (repeat 1/3)
                     (interleave (concat (range 0 8)) (repeat -7) (repeat -5))))
       (where :pitch raise)
       (all :part ::pegs)))

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

(def stomp
  (->>
    (phrase (concat (repeat 9 1/3) [5]) (concat (repeat 9 -14) [nil]))
    (times 2)
    (all :part ::beat)))

(def mid-section
  (let [b (->> (phrase (repeat 4) [0 -3 1 -2])
               (where :pitch lower)
               (all :part ::bass)
               (times 4))]
    (->> and-if-you-lived-here 
         (with youd-be-home-by-now) 
         (then (->> and-if-you-lived-here
                    (with youd-be-home-right-now)))
         (times 2)
         (with (times 4 stomp))
         (with b))))

(def extra2
  (->> (phrase (repeat 2) [-3 -2 -1 0 1 2 5])
       (then (phrase [18] [1]))
       (times 2)
       (all :part ::harmony)))
 
; Return
(def fallbass
  (->> (take-while #(-> % :time (< 4)) base)
       (then (phrase [4] [[-10.5 -6.5]]))
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

(def stompier
  (->>
    (phrase (repeat 24 1/3) (concat (repeat 12 -7) (repeat 6 -14) (repeat 6 -17.5)))
    (times 4)
    (all :part ::beat)))

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
       (with stompier)
       (then (->> (after -4 (phrase (repeat 2/3) [3.5 3 2.5 2 1 0.5]))
                  (all :part ::harmony)))))

(def bump
  (->> (phrase (concat (repeat 6 1) (repeat 6 1/3))
               (concat (repeat 6 -14) (repeat 6 -7)))
       (all :part ::beat)))

; Structure
(def ska
  (->>
    (->> base
         (then (groove 1))
         (then (groove 2))
        (where :pitch (comp F minor)))
    (then (->> mid-section
               (where :pitch (comp low B major))))
    (then (->> rise
               (then bump)
               (then (with extra (groove 2)))
               (where :pitch (comp F minor))))
    (then (->> mid-section
               (with extra2)
               (where :pitch (comp low B major))))
    (then (->> rise
               (then (->> (groove 1) (take-while #(-> % :time (< 40))) (in-time (accelerando 0 8 3/2))))
               (where :pitch (comp F minor))))
    (where :pitch temperament/equal)
    (in-time (bpm 180))))

(defmethod play-note ::harmony [{hz :pitch seconds :duration}]
  (some-> hz (corgan :vol 0.2 :under-attack 0.2 :attack 0.02 :dur seconds :wet 0.2 :room 0.8 :pan -1/5 :vibrato 3 :limit 6000))
  (some-> hz (sing :vol 0.05 :dur seconds :wet 1 :room 0.8 :pan 1/5)))

(defmethod play-note ::pegs [{hz :pitch seconds :duration}]
  (some-> hz (sing :volume 1.5 :dur seconds :wet 1 :room 0.8 :pan 1/5)))

(defmethod play-note ::bass [{hz :pitch seconds :duration open? :open secondary? :secondary}]
  (let [[pan vol] (if secondary? [1/3 0.1] [-1/3 0.3])]
   (some-> hz (corgan :vol vol :under-attack 0.1 :attack 0.001 :dur seconds :wet 0.2 :room 0.8 :pan pan :vibrato (if open? 2 1) :limit (if open? 4000 1500)))
   (some-> hz (bass :volume (* 3/4 vol) :dur seconds :wet 0.2 :room 0.8 :pan (- pan))) ))

(defmethod play-note ::rhythm [{hz :pitch, s :duration}]
  (some-> hz (organ s :attack 0.01 :vol 0.3 :limit 2000 :attack 0 :pan 1/3 :room 0.8 :wet 0.6)))

(defmethod play-note ::beat [{hz :pitch}]
  (some-> hz (drum/kick2 :amp 0.4 :noise 0.05)))

(defmethod play-note ::melody [{hz :pitch s :duration}]
  (some-> hz (sawish :pan -1/6 :volume 0.2 :duration 0.3 :vibrato 1 :wet 0.3 :room 0.01 :limit 3000 :depth 1))
  (some-> hz (harpsichord :pan 1/6 :vol 0.2 :duration s :vibrato 1 :wet 0.4 :room 0.8 :limit 2000 :depth 1)))

(defmethod play-note ::sunrise [{hz :pitch s :duration}]
  (some-> hz (organ :walk 1/2 :pan 1/2 :attack 0.1 :vol 0.1 :dur s :wet 0.5 :room 0.9 :limit 5000 :p 2))
  (some-> hz (brassy :walk 1/2 :pan 1/2 :attack 0.1 :vol 0.4 :dur s :wet 0.6 :room 0.8 :limit 4000)))

(comment
  (recording-start "ska.wav")
  (play ska)
  (recording-stop)
  (jam (var ska))
)
