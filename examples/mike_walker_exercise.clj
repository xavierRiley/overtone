(ns examples.mike-walker
    (:use [overtone.live]))

(defn rotate-while
  "Rotates a collection left while (pred item) is true. Will return a
   unrotated sequence if (pred item) is never true. Executes in O(n) time."
  [pred coll]
  (let [head (drop-while pred coll)]
    (take (count coll) (concat head coll))))

(defn get-dir [x]
  (let [penul (note (first (rest (reverse x)))) final (note (last x))]
  (println penul final)
  (cond
    (> penul final) :down
    (> final penul) :up)))

(defn continue-scale-from-prev [current-scale prev-scale]
  (let [direction (get-dir prev-scale) last-note (last prev-scale)]
    (if (= direction :up)
      (rotate-while (fn [x] (>= (note x) (note last-note))) (scale-from current-scale))
      (rotate-while (fn [x] (>= (note x) (note last-note))) (scale-from (reverse current-scale)) ))))

(defn scale-from [n &{:keys [beats prev-scale] :or {beats 32} }]
  (let [current-scale (take beats (cycle (concat (vec n) (reverse (subvec (vec n) 1 (- (count n) 1))))))]
    (concat
      prev-scale
      (if (not (empty? prev-scale))
        (continue-scale-from-prev n prev-scale)
        current-scale))))

(defn scale-between [low-note high-note note-to-test]
  (and (>= note-to-test low-note) (<= note-to-test high-note)))

(definst harpsichord [freq 440]
  (let [duration 1]
    (*
      (line:kr 1 1 duration FREE)
      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
             1 1 (/ 1 freq) (* duration 2) 0.25))))

(->>
  (scale-from [:F1 :G1 :A1 :Bb1 :C2 :D2 :E2 :F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3])
  (scale-from [:F#1 :G#1 :A1 :B1 :C#2 :D2 :E2 :F#2 :G#2 :A2 :B2 :C#3 :D3 :E3 :F#3 :G#3 :A3] :prev-scale)
  (scale-from [:F1 :G1 :Ab1 :Bb1 :C2 :D2 :Eb2 :F2 :G2 :Ab2 :Bb2 :C3 :D3 :Eb3 :F3 :G3 :Ab3] :prev-scale))

(defn octave [midi-scale]
  (map (fn [x] (+ 12 x)) midi-scale))

(def melody
  (let [pitches
    (->>
      (scale-from [:F1 :G1 :A1 :Bb1 :C2 :D2 :E2 :F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3])
      (scale-from [:F#1 :G#1 :A1 :B1 :C#2 :D2 :E2 :F#2 :G#2 :A2 :B2 :C#3 :D3 :E3 :F#3 :G#3 :A3] :prev-scale)
      (scale-from [:F1 :G1 :Ab1 :Bb1 :C2 :D2 :Eb2 :F2 :G2 :Ab2 :Bb2 :C3 :D3 :Eb3 :F3 :G3 :Ab3] :prev-scale))
    durations
     (repeat (count pitches) 1)
    times (reductions + 0 durations)]
    (map vector times pitches)))

(note :F3)

(defn play [metro notes] 
  (let [play-note (fn [[beat pitch]] (at (metro beat) (-> pitch note midi->hz harpsichord)))]
    (dorun (map play-note notes)))) 

(defn play-round [metro notes]
  (let [after (fn [beats metro] (comp metro #(+ % beats)))]
    (play metro notes)
    (play (after 4 metro) notes)
    (play (after 8 metro) notes)
    (play (after 16 metro) notes)))

(play (metronome 140) melody)
;(play-round (metronome 120) melody)

