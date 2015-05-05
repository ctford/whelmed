(ns whelmed.instrument
  (:use
    [leipzig.melody]
    [overtone.live]))

; Generic machinery
(defsynth walker [out-bus 0 freq 0.5]
  (out:kr out-bus (lf-noise1:kr freq)))
(defonce random-walk (audio-bus))
(defonce walk (walker random-walk))
(def resonance (mul-add (in:kr random-walk) 1500 2000))
(def space (mul-add (in:kr random-walk) 0.5 0.4))
 
(defcgen cut-out [input {:default :none}]
  (:ar (do (detect-silence input :action FREE)
           input))
  (:default :ar))

(defcgen effects [input  {:default :none}
                  pan    {:default 0}
                  wet    {:default 0.33}
                  room   {:default 0.5}
                  volume {:default 1.0}
                  early  {:default 0.1}
                  high   {:default 20000}
                  low    {:default 0}]
  (:ar (-> input
           (* volume)
           (pan2 pan)
           (free-verb :mix early :room 0.1)
           (free-verb :mix wet :room space)
           (lpf high)
           (hpf low)
           cut-out))
  (:default :ar))

; Instruments
(definst shudder [freq 440 vibrato 6 pan 0 wet 0.5 volume 1.0 room 0.5]
  (-> (square freq)
      (* (sin-osc freq))
      (* (sin-osc vibrato))
      (* 2/3 (env-gen (perc 2 1.5)))
      (effects :room room :wet wet :pan (line:ar pan (- pan) 3.5) :volume volume)))

(definst sawish [freq 440 duration 1.5 vibrato 8/3 depth 1 volume 1.0 pan 0.0 wet 0.5 room 0.5]
  (let [envelope (env-gen (perc 0.01 duration))]
    (-> (sin-osc (* freq 0.51))
        (+ (* 3 (sin-osc freq)))
        (clip2 0.5)
        (* 4/3 envelope)
        (rlpf (mul-add (sin-osc vibrato) (* freq depth) (* 2 freq)) 1/3)
        (effects :room room :wet wet :pan pan :volume volume))))

(definst groan [freq 440 duration 10 vibrato 8/3 volume 1.0 position 0 wet 0.5 room 0.5 low 0.25 limit 3000]
  (let [envelope (* (sin-osc vibrato) (env-gen (perc 0.1 duration)))]
    (-> (+
         (* low (sin-osc (* freq 1/2)))
         (* (sin-osc 0.5) (+ 0.1 (saw freq)))
         (* (sin-osc 0.8) (+ -0.03 (square freq)))
         (+ -0.04 (sin-osc freq)))
        (* 0.7 envelope)
        (effects :room room :wet wet :pan position :volume volume :high limit))))

(definst bell [frequency 440 duration 1.0 volume 1.0 position 0 wet 0.5 room 0.5
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2 h5 0.15]
  (let [harmonics   [ 1  2  3  4.2  5.4 6.8]
        proportions [h0 h1 h2   h3   h4  h5]
        proportional-partial
        (fn [harmonic proportion]
          (let [envelope (* 1/5 (env-gen (perc 0.01 (* proportion duration))))
                overtone (* harmonic frequency)]
            (* 1/2 proportion envelope (sin-osc overtone))))
        partials
        (map proportional-partial harmonics proportions)
        whole (* 10 (mix partials))]
    (effects whole :room room :wet wet :pan position :volume volume)))

(definst brassy [freq 440 dur 1.0 vol 1 wet 0.5 room 0.5 noise 1.0 limit 3000 p 1]
  (-> (+
       (* (sin-osc freq) (env-gen (adsr 0.0 0.3 0.3)))
       (* (white-noise) noise (env-gen (perc 0.0 0.01))))
      (* vol)
      (rlpf (* 5 freq) 1/10)
      (* (pulse p 2/3))
      (clip2 0.3)
      (effects :room room :wet wet :pan (line:kr -1 1 dur) :volume vol :high limit)))

(definst organ [freq 440 dur 1.0 vol 1.0 pan 0.0 wet 0.5 room 0.5 limit 20000 attack 0.1]
  (->
    (map #(sin-osc (* freq %)) (range 1 5))
    mix
    (* (env-gen (asr attack 1.0 0.5) (line:kr 1.0 0.0 dur)))
    (lpf (mul-add (sin-osc 5) freq (* freq 5)))
    (effects :pan pan :wet wet :room room :volume vol :high limit)))

(definst corgan [freq 440 dur 1.0 depth 1 walk 1 attack 0.01 under-attack 0.3 vol 1.0 pan 0.0 wet 0.5 room 0.5 vibrato 3 limit 99999]
  (->
    (saw freq)
    (* 99)
    (rlpf (mul-add (sin-osc vibrato) (line:kr 0 (* depth resonance) 10) (* freq 4)) 1/20)
    (clip2 0.4)
    (* (env-gen (adsr attack 1.0 0.5) (line:kr 1.0 0.0 dur)))
    (+ (* 1/4 (sin-osc (* 1.002 freq)) (env-gen (perc under-attack dur))))
    (rlpf (* walk resonance) 1/5)
    (effects :pan pan :wet wet :room room :volume vol :high limit)))

(definst kraft-bass [freq 440 dur 1.0 vol 1.0 pan 0 wet 0.5 room 0.5]
  (let [envelope (env-gen (asr 0 1 1) (line:kr 1.0 0.0 dur))
        level (+ 100 (env-gen (perc 0 3) :level-scale 6000))
        osc (mix [(saw freq)
                  (saw (* freq 1.005))
                  (pulse (/ freq 2) 0.5)])]
    (-> osc
        (lpf level)
        (* envelope)
        (effects :pan pan :wet wet :room room :volume vol))))

(definst bass [freq 110 dur 1.0 res 1000 volume 1.0 pan 0 wet 0.5 room 0.5]
  (-> (sin-osc freq) 
      (+ (* 1/3 (sin-osc (* 2 freq))))
      (+ (* 1/2 (sin-osc (* 3 freq))))
      (+ (* 1/3 (sin-osc (* 5 freq))))
      (clip2 0.8)
      (rlpf res 1/7)
      (* (env-gen (adsr 0.02 0.2 0.1 0.1) (line:kr 1 0 dur)))
      (effects :pan pan :wet wet :room room :volume volume)))

(definst organic [freq 440 dur 1 volume 0.6 pan 0 wet 0.5 room 0.5]
  (-> (square freq)
      (+ (sin-osc 9) (sin-osc (* 2 freq)))
      (+ (sin-osc 9) (sin-osc (* 1.999 freq)))
      (+ (sin-osc 6) (sin-osc (* 4.01 freq)))
      (+ (sin-osc 3) (sin-osc (* 6 freq)))
      (+ (sin-osc 3) (sin-osc (* 1/2 freq)))
      (* 1/10 (env-gen (adsr 0.05 0.2 0.7 0.1) (line:kr 1 0 dur)))
      (effects :pan pan :wet wet :room room :volume volume :high 4000)))

(definst sing [freq 440 dur 1.0 volume 1.0 pan 0 wet 0.5 room 0.5]
  (-> (saw freq)
      (+ (saw (* freq 1.01)))
      (rlpf (mul-add (sin-osc 8) 200 1500) 1/8)
      (* 1/4 (env-gen (asr 0.03 0.3 0.1) (line:kr 1 0 dur)))
      (effects :room room :mix wet :pan pan :volume volume)))

(definst kluck [freq 220 volume 1.0 wet 0.5 room 0.1 pan 0]
  (-> (line:kr freq (* freq 1/2) 0.5)
      sin-osc 
      (+ (sin-osc freq))
      (+ (sin-osc (/ freq 2) (sin-osc 1)))
      (* (env-gen (perc 0.01 0.1)))
      (effects :room room :wet wet :pan pan :volume volume)))

(definst tip [freq 110 volume 1.0 wet 0.5 room 0.1 pan 0]
  (-> (brown-noise)
      (+ (sin-osc (* 1/4 freq)))
      (rlpf (* 3 freq) 1/2)
      (* (env-gen (perc 0.01 0.05)))
      (effects :room room :wet wet :pan pan :volume volume)))
