(ns whelmed.songs.west
  (:require 
    [leipzig.melody :refer :all]
    [leipzig.live :as live]
    [leipzig.temperament :as temperament]
    [whelmed.melody :refer :all]
    [leipzig.scale :as scale]
    [leipzig.chord :as chord]
    [whelmed.instrument :refer :all]
    [leipzig.canon :as canon]
    [overtone.inst.drum :as drums]
    [overtone.live :as overtone]))

(def progression
  (map (partial chord/root chord/seventh) [0 (lower 4) (lower 5) (lower 2)]))

; Accompaniment
(def backing
  (let [lefts [true false true false]
        render-chord (fn [[left? chord]] (->> (phrase [4] [chord]) (all :left? left?)))]
    (->>
      progression
      (map vector lefts)
      (mapthen render-chord)
      (all :part ::accompaniment))))

(defn vary [f notes]
  (->> notes (then (f notes))))

; Lead
(def ill-run-away
  (->>
    (after -1/2
           (phrase
             [1/2 1/4 1/4 1/2 3]
             [  3   4   3   4 nil]))
    (vary (partial but 1/4 1/2 (phrase [1/4] [6])))))

(def my-heart-will-go-west-with-the-sun
  (->> (phrase
         [1/2 3/4 3/4 2/4 3/4 3/4 1/4 17/4]
         [  3   4   3   2   4   3   2   -1])
       (after -1/2)))

(def west-with-the-west-with-the 
  (->> (after -1/2 (phrase [1/2] [3]))
       (then (times 4 (phrase [3/4 3/4 2/4] [4 3 2])))))

(def theme
  (->>
    ill-run-away
    (then my-heart-will-go-west-with-the-sun)
    (all :part ::lead)))

(def spilling-theme
  (->>
    ill-run-away
    (then west-with-the-west-with-the)
    (all :part ::lead)))

(def shifted-theme
  (->>
    ill-run-away
    (times 2)
    (after 1/2)
    drop-last
    (all :part ::lead)))

; Response
(def a-parting-kiss
  (phrase
    [1/4 1/4 1/4 3/4 10/4]
    [  4   3   4   6    4]))

(def like-fairy-floss
  (with (after -1/4 (phrase [1/4] [3])) a-parting-kiss))

(def dissolves-on-the-tip-of-my-tongue
  (->>
    (phrase
      [1/4 3/4 13/4]
      [  4   6    4])
    (after -1/4)))

(def reply
 (->>
   a-parting-kiss
   (then like-fairy-floss)
   (then (times 2 dissolves-on-the-tip-of-my-tongue)) 
   (all :part ::response)))

; Break
(def consider-this
  (after -3/2
     (phrase
       [1/2 1/2 1/2 8/2]
       [  4   9   8   7])))

(def consider-that
  (->> consider-this
       (but 0 1/2 (phrase [8/2] [6]))))

(def consider-everything
  (->> consider-this
       (but 0 8
            (phrase
              [2/2 1/2 2/2 2/2 9/2]
              [  7   8   7   6   4]))))

(def break
 (->>
   consider-this
   (then consider-that)
   (then consider-everything)
   (canon/canon (canon/interval -7))
   (all :part ::break)))

; Bass
(def light-bass
  (->> progression
       (map :i progression)
       (phrase (repeat 4))
       (where :pitch lower)
       (all :part ::bass)))

(def bass
  (->> light-bass
       (canon/canon
         (comp (canon/simple 1)
               (canon/interval 6)
               (partial where :duration dec)
               (partial all :left? true)))))

(def beat
  (->> (times 4 (phrase [6/4 4/4 6/4] (repeat -14)))
       (with (times 2 (phrase [1 2 2 2 1/2 1/2] (cons nil (repeat -10)))))
       (all :part ::kick)))

(def flat-beat
  (->> (phrase (repeat 4 1) (repeat -14))
       (times 4)
       (all :part ::kick)))

(def beat2
  (->> (phrase [1 1 1/4 3/4 1 1/4 1/4 1/2 1/2 1/4 1/4 1 1/2] (cycle [-7 -3]))
       (with (after 4 (phrase [3/2 3/2 1] [-8 -10 -12])))
       (times 2)
       (with beat)
       (all :part ::kick)))

; Body
(def west-with-the-sun
  (let [accompaniment
          (->> backing (with bass)) 
        intro
         (->> backing (then accompaniment))
        call
          (->> theme (with accompaniment beat) (times 2))
        response
          (->> reply (with accompaniment beat2) (times 2))
        variation
          (->> theme (then spilling-theme)
            (with (->> (with beat accompaniment) (times 2))))
        fadeout
          (->> accompaniment (with shifted-theme beat) (then bass))]
    (->>
      intro (then call) (then response)
      (then (->> break (with light-bass flat-beat) (times 2)
              (with (->> backing (after 16)))))
      (then variation)
      (then (->> response (with (->> break (after 16)))))
      (then fadeout)
      (where :pitch (comp temperament/equal scale/A scale/minor))
      (where :time (bpm 80))
      (where :duration (bpm 80)))))

; Arrangement
(defmethod play-note ::bass
  [{freq :pitch left? :left?}]
  (let [[position low] (if left? [-1/3 0.3] [1/5 2])]
    (some-> freq (groan :volume 0.5 :position position :wet 0.3 :low low))))

(defmethod play-note ::accompaniment
  [{freq :pitch left? :left?}]
  (some-> freq (shudder :volume 1 :pan (if left? 1/2 -1/2) :wet 0.8)))

(defmethod play-note ::lead
  [{freq :pitch}]
  (some-> freq (sawish :pan -1/6 :vibrato 8/3 :wet 0.7 :volume 1)))

(defmethod play-note ::response
  [{freq :pitch seconds :duration}]
  (some-> freq (organ seconds 3 :vol 1.0 :pan -1/4 :wet 0.8)))

(defmethod play-note ::break
  [{freq :pitch}]
  (some-> freq (bell 2 :duration 10 :vol 1.5 :position -1/6 :wet 0.8)))

(defmethod play-note ::kick
  [{freq :pitch}]
  (some-> freq drums/kick2))

(comment
  (overtone/fx-freeverb)
  (->> west-with-the-sun var live/jam)
  (->> west-with-the-sun live/play)
  (overtone/recording-start "west-with-the-sun.wav")
  (overtone/recording-stop)
) 
