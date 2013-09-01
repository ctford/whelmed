(ns whelmed.instrument
  (:use
    [leipzig.melody]
    [overtone.live])
  (:require [overtone.synth.stringed :as strings])) 

(definst shudder [freq 440 vibrato 6]
  (let [envelope (env-gen (perc 2 1.5) :action FREE)]
    (*
      (* envelope (sin-osc vibrato))
      (square freq)
      (sin-osc freq))))

(definst sawish [freq 440]
  (let [envelope (env-gen (perc 0.2 1.5) :action FREE)]
    (*
      0.7
      envelope
      (+
        (square (* freq 0.99))
        (square freq)))))

(definst sinish [freq 440]
  (let [envelope (env-gen (perc 0.1 1.1) :action FREE)]
    (*
      envelope
      (sin-osc freq))))

(definst groan [freq 440 duration 10000 vibrato 8/3 volume 1.0]
  (let [length (/ duration 1000)
        envelope (* (sin-osc vibrato)
                    (env-gen (perc 0.1 length) :action FREE))]
    (*
     0.7
     volume
     envelope
     (+
      (* (sin-osc 0.5) (+ 0.1 (saw freq)))
      (* (sin-osc 0.8) (+ -0.03 (square freq)))
      (+ -0.04 (sin-osc freq))))))

(definst bell [frequency 440 duration 1000
  h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics   [ 1  2  3  4.2  5.4 6.8]
        proportions [h0 h1 h2   h3   h4  h5]
        proportional-partial
          (fn [harmonic proportion]
            (let [envelope
                    (env-gen (perc 0.01 (* proportion (/ duration 1000))))
                  overtone
                    (* harmonic frequency)]
              (* 1/2 proportion envelope (sin-osc overtone))))
        partials
          (map proportional-partial harmonics proportions)
        whole (mix partials)]
      (detect-silence whole :action FREE)
      whole))

(definst sawnoff [freq 440 depth 10]
  (let [envelope (env-gen (perc 0.1 0.9) :action FREE)] 
    (* 
      envelope
      (sin-osc freq)
      (sin-osc (* 2 freq))
      (saw (+ freq (* depth (lf-saw:kr 0.1 0.2)))))))

(strings/gen-stringed-synth ektara 1 true)
(defn pick [distort amp {midi :pitch, start :time, length :duration}]
  (let [synth-id (at start
         (ektara midi :distort distort :amp amp :gate 1))]
    (at (+ start length) (ctl synth-id :gate 0))))

(definst brassy [freq 440 dur 1000 vol 1 growl 1]
  (lpf
    (* vol
    (+
      (* 1/1 (sin-osc (* 1 freq)) 
         (env-gen (adsr 0.2 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1000)) :action FREE))
      (* 1/2 (sin-osc (* 3 freq)) 
         (env-gen (adsr 0.4 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1000)) :action FREE))
      (* 1/3 (sin-osc (* 5 freq)) 
         (env-gen (adsr 0.4 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1000)) :action FREE))
      (* 1/4 (sin-osc (* 7 freq)) 
         (env-gen (adsr 0.4 0.3 0.2) (line:kr 1.0 0.0 (/ dur 1000)) :action FREE)))) 
     (+ (* 4 freq) (* (line:kr (* growl 4) 1 0.5) freq (sin-osc 50)))))


(definst open [duration 1000 volume 1.0]
  (let [low (lpf (pink-noise) 5000)
        hi (hpf low 3000)
        env (line 1 0 (/ duration 1000) :action FREE)
        dive (lpf hi (* env 8000))]
    (* 3 volume env dive)))

(definst kick [volume 1.0]
  (let [freq 50
        fenv (* (env-gen (envelope [3 1] [0.02] :exp)) freq)
        aenv (env-gen (perc 0.005 0.5) :action FREE)]
    (* volume (sin-osc fenv (* 0.5 Math/PI)) aenv))) 

(definst woah [freq 440 duration 1000 volume 1.0]
  (let [fenv (* (env-gen (perc 0.1 (/ duration 1000))) freq)
        aenv (env-gen (perc 0.005 (/ duration 1000)) :action FREE)]
    (* volume (sin-osc fenv (* 0.5 Math/PI)) aenv)))

(definst click [volume 1.0]
  (let [envelope (env-gen (perc 0.05 0.2) :action FREE)]
    (* volume envelope (pulse 5000 100))))

(definst organ [freq 440 dur 1000 vol 1.0]
  (* 1/6 vol
     (env-gen (asr 0.1 1.0 0.5)
       (line:kr 1.0 0.0 (/ dur 1000))
       :action FREE)
     (mix (map #(sin-osc (* freq %)) (range 1 5)))))
