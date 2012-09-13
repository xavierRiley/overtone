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
    (println direction last-note)
    (if (= direction :up)
      (rotate-while (fn [x] (>= (note x) (note last-note))) current-scale)
      (rotate-while (fn [x] (>= (note x) (note last-note))) (reverse current-scale) ))))

(defn scale-from [n &{:keys [beats prev-scale] :or {beats 32} }]
  (let [current-scale (take beats (cycle (concat (vec n) (reverse (subvec (vec n) 1 (- (count n) 1))))))]
    (concat
      prev-scale
      (if (not (empty? prev-scale))
        (continue-scale-from-prev current-scale prev-scale)
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

(.printStackTrace *e)

(scale-from [:F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3] :beats 32)

(def melody
  (let [pitches
         (vec (scale-from [:F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3]))
        durations
         [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
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

(play (metronome 140) 
  (concat 
    (map vector (range 32) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :f :major)) 32))
    (map vector (range 32 64) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :Ab :major)) 32))
    (map vector (range 64 96) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :Bb :major)) 32))
    (map vector (range 96 128) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :C :major)) 32))
    (map vector (range 128 160) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :Gb :major)) 32))
    (map vector (range 160 192) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :D :major)) 32))
    (map vector (range 192 224) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :Eb :major)) 32))
    (map vector (range 224 256) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :A :major)) 32))
    (map vector (range 288 320) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :B :major)) 32))
    (map vector (range 320 352) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :G :major)) 32))
    (map vector (range 352 384) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :Db :major)) 32))
    (map vector (range 384 416) (scale-from (filter #(scale-between (note :F2) (note :A4) %) (scale-field :E :major)) 32))
  ))
;(play-round (metronome 120) melody)

