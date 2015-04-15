(ns whelmed.songs.west
  (:use
    [leipzig.melody]
    [leipzig.live]
    [leipzig.temperament]
    [whelmed.melody]
    [leipzig.scale]
    [leipzig.chord]
    [whelmed.instrument])
  (:require 
    [leipzig.canon :as canon]
    [overtone.inst.drum :as drums]
    [overtone.live :as overtone]))

(def progression
  (map (partial root seventh) [0 (lower 4) (lower 5) (lower 2)]))

; Accompaniment
(def backing
  (let [render-chord (fn [[left? chord]] (->> (phrase [4] [chord]) (all :left? left?)))]
    (->>
      progression
      (map vector [true false true false])
      (mapthen render-chord)
      (all :part ::accompaniment))))

; Lead
(def ill-run-away
  (->>
   (phrase
     [1/2 1/4 1/4 1/2]
     [  3   4   3   4])
  (after -1/2)))

(def ill-get-away
  (->> ill-run-away
    (wherever (between? 1/4 1/2), :pitch (from 3))))

(def my-heart-will-go-west-with-the-sun
  (after -1/2
     (phrase
       [1/2 3/4 3/4 2/4 3/4 3/4 1/4 17/4]
       [  3   4   3   2   4   3   2   -1])))

(def west-with-the-west-with-the 
  (let [west-with-the
          (->> my-heart-will-go-west-with-the-sun
               (take 4) (drop 1) (times 4))]
  (->>
    [{:time -1/2 :pitch 3 :duration 1/2}]
    (then west-with-the))))

(def theme
  (->>
    ill-run-away
    (then (after 3 ill-get-away))
    (then (after 3 my-heart-will-go-west-with-the-sun))
    (where :part (is ::lead))))

(def half-theme
  (let [crippled-theme
          (->> ill-run-away
               (then (->> ill-get-away (after 3))))]
    (->>
      (after 1/2 crippled-theme)
      (then (after 3 crippled-theme))
      (where :part (is ::lead)))))

(def spilling-theme
  (->>
    ill-run-away
    (then (after 3 ill-get-away))
    (then (after 3 west-with-the-west-with-the))
    (where :part (is ::lead))))

; Response
(def a-parting-kiss
  (phrase
    [1/4 1/4 1/4 3/4 10/4]
    [  4   3   4   6    4]))

(def like-fairy-floss
  (with [{:time -1/4 :pitch 3 :duration 1/4}]
        a-parting-kiss))

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
   (then dissolves-on-the-tip-of-my-tongue) 
   (then dissolves-on-the-tip-of-my-tongue)
   (where :part (is ::response))))

; Break
(def consider-this
  (after -3/2
     (phrase
       [1/2 1/2 1/2 8/2]
       [  4   9   8   7])))

(def consider-that
  (->> consider-this
    (wherever (between? 0 1/2), :pitch dec)))

(def consider-everything
  (->>
    (take 3 consider-this)
    (then
      (phrase
        [2/2 1/2 2/2 2/2 9/2]
        [  7   8   7   6   4]))))

(def breakdown
 (->>
   consider-this
   (then consider-that)
   (then consider-everything)))

(def breakup (->> breakdown (where :pitch lower)))
(def break
  (->>
    (with breakup breakdown)
    (where :part (is ::break))))

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
       (with (times 2 (->> (phrase [2 2 2 1/2 1/2] (repeat -10)) (after 1))))
       (all :part ::kick)))

(def flat-beat
  (->> (phrase (repeat 4 1) (repeat -14))
       (times 4)
       (all :part ::kick)))

(def beat2
  (->> (phrase [1 1 1/4 3/4 1 1/4 1/4 1/2 1/2 1/4 1/4 1 1] (cycle [-7 -3]))
       (with (after 4 (phrase (cycle [3/2]) [-8 -10 -12])))
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
          (->> accompaniment (with half-theme beat) (then bass))]
    (->>
      intro (then call) (then response)
      (then (->> break (with light-bass flat-beat) (times 2)
              (with (->> backing (after 16)))))
      (then variation)
      (then (->> response (with (->> break (after 16)))))
      (then fadeout)
      (where :pitch (comp equal A minor))
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
  (some-> freq (organ seconds 3 :vol 1.5 :pan -1/4 :wet 0.6)))

(defmethod play-note ::break
  [{freq :pitch}]
  (some-> freq (bell 2 8 :vol 1.5 :position -1/6 :wet 0.3)))

(defmethod play-note ::kick
  [{freq :pitch}]
  (some-> freq drums/kick2))

(comment
  (overtone/fx-freeverb)
  (->> west-with-the-sun var jam)
  (->> west-with-the-sun play)
  (overtone/recording-start "west-with-the-sun.wav")
  (overtone/recording-stop)
) 
