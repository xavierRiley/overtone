(ns examples.mike-walker
    (:use [overtone.live]))

(defn scale-from [n]
  (cons n (lazy-seq (scale-from (concat (rest (reverse n)) n)))))

(defn scale-from [n]
  (take 24 (cycle (concat (vec n) (reverse (subvec (vec n) 1 (- (count n) 1)))))))

(defn scale-from [n]
  (take 24 (cycle (concat (vec n) (reverse (subvec (vec n) 1 (- (count n) 1)))))))

(scale-from [:F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3])

(definst harpsichord [freq 440]
  (let [duration 1]
    (*
      (line:kr 1 1 duration FREE)
      (pluck (* (white-noise) (env-gen (perc 0.001 5) :action FREE))
             1 1 (/ 1 freq) (* duration 2) 0.25))))

(def melody
  (let [pitches
         (vec (scale-from [:F2 :G2 :A2 :Bb2 :C3 :D3 :E3 :F3 :G3 :A3]))
        durations
         [1 1 2/3 1/3 1
          2/3 1/3 2/3 1/3 2
          1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3 1/3
          2/3 1/3 2/3 1/3 2]
        times (reductions + 0 durations)]
    (map vector times pitches)))

(defn play [metro notes] 
  (let [play-note (fn [[beat pitch]] (at (metro beat) (-> pitch note midi->hz harpsichord)))]
    (dorun (map play-note notes)))) 

(defn play-round [metro notes]
  (let [after (fn [beats metro] (comp metro #(+ % beats)))]
    (play metro notes)
    (play (after 4 metro) notes)
    (play (after 8 metro) notes)
    (play (after 16 metro) notes)))

;(play (metronome 120) melody)
;(play-round (metronome 120) melody)

