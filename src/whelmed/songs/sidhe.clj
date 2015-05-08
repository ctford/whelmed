(ns whelmed.songs.sidhe
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.canon]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument])
  (:require [overtone.live :as overtone]
            [overtone.inst.drum :as drums]
            [overtone.synth.stringed :as string]
            [leipzig.temperament :as temperament]))


; First section
(def bassline
  (->> (phrase (repeat 4) (range 0 -8 -1))
       (where :pitch lower)))

(def dux
  (let [a (phrase
            (mapcat repeat [8 1 1 4 1] [1 3/2 1/2 1 3/2])
            [0 1 2 0 1 2 3 1 3 3 2 1 0 -1/2 1])
        b (phrase
            [1/2 3/2 1/2 1 1 1 1 3/2 1/2 3/2 1/2 3/2 1/2 1 1 2]
            [2 3 3 2 1 0 -1 -3 -1 -3 -1 -3 -1 -3 -5 -7])]
    (->> a (then b))))

(def comes
  (let [a (phrase [1 1 1 1 1 1 1 1 3/2 1/2 1/2 1/2 1/2 1/2 7/2]
                  [0 1 2 0 -1 0 1 -1 -2 -3 -4 -2 0 3 1])
        b (phrase (concat (repeat 25 1/2) [1 1 1/2 1/2 1/2 1/2])
                  [1 -4 -2 0 3 -4 -2 0 3 -5 -3 -1 2 -5 -3 -1 2 -3 -1 1 -3 1 2 1 -1 0 2 1 0 -1 -3]) ]
    (->> a (then b))))

(def first-section
  (let [dux (all :part ::starboard dux)
        comes (all :part ::port comes)
        bassline (all :part ::fore bassline)]
    (->> (with dux (drop 4 comes))
         (then (with dux comes bassline)))))


; Second section
(def harmony
  (->> bassline 
       (where :pitch (from 9))
       (wherever #(-> % :time (= 12)) :pitch (from 1/2))))

(def line 
  (let [first-flourish (phrase
                         [1/4 1/4 3/2 1 1 9/2]
                         [1 2 3 4 2 1])
        second-flourish (phrase (map :duration first-flourish)
                                [1 2 3 2 1 0])]
    (->>
      (phrase [5/2 1/2 1/2 8/2] [4 3 2 4])
      (then first-flourish)
      (then (phrase [5/2 1/4 1/4 9/2] [4 2 3 4]))
      (then second-flourish))))

(def beat 
  (->> (phrase [3/2 1/2 1/2 3/2] (repeat -14))
       (times 8)))

(def beatback
  (->> beat
       (with (times 4 (phrase (repeat 16 1/2) (cycle [-12 -10]))))))

(def second-section
  (->>
    (with
      (all :part ::fore bassline)
      (all :part ::port line)
      (all :part ::starboard harmony)
      (all :part ::aft beat))
    (then
      (with
        (all :part ::fore bassline)
        (all :part ::port harmony)
        (all :part ::starboard line) 
        (all :part ::aft beatback))))) 


; Key change
(def descent
  (->> (phrase (cycle [4 4 8]) [0 -3 -4 -1 -1.5 -6])
       (where :pitch lower)))

(def drift
  (phrase (cycle [3 1 3 1 8])
          [0 2 1 -1 0 1 -1 0.5 -1.5 1]))

(def bubble
  (letfn [(riff [z y x] (phrase (repeat 1/2) [z z x z z x z z]))]
    (->>
      (riff 4 2 -1)
      (then (riff 4 1 -1))
      (then (times 2 (riff 3 0 -2)))
      (then (riff 3 1 -1))
      (then (riff 2.5 0.5 -1.5))
      (then (times 2 (riff 3 1 -1.5))))))

(def funk
  (->> (phrase [3/2 2/2 1/2 1/2 1/2] (cycle [-14 -14 [-10 -7] [-10 -7] [-10 -7]]))
       (times 4)))

(def key-change
  (with
    (all :part ::fore descent)
    (all :part ::port drift)
    (all :part ::starboard bubble)
    (all :part ::aft (times 2 funk))))

; Third section
(def dunk
  (->> funk
       (with (phrase (repeat 16 1/2) (cycle [nil -3])))
       (times 4)))

(def circuit
  (->> (phrase [4 4 8] [-2 -3 0])
       (times 4)
       (where :pitch lower)))

(def trace
  (->> (phrase [4 4 2 2 4]
               [2 -0.5 2 1 0])
       (then (phrase [4 4 8]
               [2 -0.5 2])) 
       (times 2)))

(def flutter
  (->> (phrase [4 4 2 2 4]
               [4 1 4 3 2])
       (then (phrase [4 4 8]
                     [4 -0.5 -3]))
       (times 2)))

(def third-section 
  (with
    (all :part ::fore circuit)
    (all :part ::port trace)
    (all :part ::starboard flutter)
    (all :part ::aft dunk)))

; Finale
(def uplift
  (phrase (concat (repeat 7 4) [20]) [4 3 4 3 2 4 4 4 4]))

(def peace
  (->> harmony
       (but 28 32 (phrase [36] [-5]))))

(def finale 
  (with
    (all :part ::fore bassline)
    (all :part ::port peace)
    (all :part ::starboard uplift)))

; Arrangement
(defmethod play-note ::port [{hz :pitch seconds :duration direction :direction}]
  (some-> hz (corgan seconds :vol 0.4 :pan -1/2 :wet 0.3 :room 0.9 :vibrato 100/60)))

(defmethod play-note ::starboard [{hz :pitch seconds :duration direction :direction}]
  (some-> hz (corgan seconds :vol 0.4 :pan 1/2 :wet 0.5 :room 0.9 :vibrato 100/15)))

(defmethod play-note ::fore [{hz :pitch seconds :duration direction :direction}]
  (some-> hz (kraft-bass :vol 1 :dur seconds :pan 0 :wet 0.7 :room 0.9)))

(defmethod play-note ::aft [{hz :pitch}] (some-> hz (drums/kick2 :amp 0.4)))


; Structure
(def sidhe 

  "A young man once a wand'ring went,
  He deemed he'd come of age,
  He severed every earthly tie,
  And curses his home a cage,
  
  His tracks, which trailed the setting sun,
  A thousand furlongs spanned,
  Pressed in clay and marked in mud,
  And printed on the sand,
  
  At last he reached the border peaks,
  With which the world is crowned,
  He sought a tarn to bathe his feet,
  And such a tarn he found,
  
  'The mountain cups his gnarléd hand,
  To pool the liquid sky,
  Some vain goddess's looking glass,
  Let fall and then let lie',
  
  So read his later vain attempts,
  To capture with his pen,
  A crystel well of wilderness,
  Transcending mortal ken,
  
  Yet as he stood in reverie,
  The nightfall stole around,
  Stars unsheathed above his head,
  And mist rose from the ground,
  
  And then, reflected in the tarn,
  The starlight coalesced,
  Our young man's ideal fantasy,
  Of loveliness, expressed,

  Her skin shone white as angel's robes, 
  Her hair swirled black as pitch, 
  Two shards of ice comprised her eyes,
  Two drops of blood - her lips.
  
  Yet as he bent to better see,
  His touch disturbed the water,
  Her face dissolved and left him only,  
  Faint and distant laughter.

  ***
  
  Now memory fades, unwanted thoughts,
  Disturb him in the dark,
  Was she merely fancy spun,
  To sooth a lonely heart?"

  (->>
    (->> first-section
         (then second-section)
         (then key-change)
         (where :pitch (comp C minor)))
    (then
      (->> third-section
           (then first-section)
           (then second-section)
           (then key-change)
           (where :pitch (comp D minor))))
    (then
      (->> third-section
           (then first-section)
           (then finale)
           (where :pitch (comp E minor))))
    (where :pitch temperament/equal)
    (in-time (comp (bpm 80) (accelerando 0 16 4/5)))))

(comment
  (overtone/recording-start "sidhe.wav")
  (play sidhe)
  (-> sidhe var jam)
  (overtone/recording-stop))
