(ns wavelet-chord.chroma)

(def pitch-classes
  ["C" "C#" "D" "D#" "E" "F"
   "F#" "G" "G#" "A" "A#" "B"])

(defn frequency-to-midi
  "Converts a frequency in Hz to its nearest MIDI note."
  [frequency]
  (when (pos? frequency)
    (int
      (Math/round
        (+ 69.0
           (* 12.0
              (/ (Math/log (/ frequency 440.0))
                 (Math/log 2.0))))))))

(defn frequency-to-pitch-class
  "Maps a frequency to one of the 12 musical pitch classes."
  [frequency]
  (when-let [midi-note (frequency-to-midi frequency)]
    (mod midi-note 12)))

(defn normalize-chroma
  "Normalizes a 12-dimensional chroma vector."
  [chroma]
  (let [total (reduce + chroma)]
    (if (pos? total)
      (mapv #(/ (double %) total) chroma)
      (vec chroma))))

(defn create-empty-chroma []
  (vec (repeat 12 0.0)))

(defn add-to-chroma
  "Adds a magnitude to the corresponding pitch class."
  [chroma frequency magnitude]
  (if-let [pitch-class (frequency-to-pitch-class frequency)]
    (update chroma pitch-class + (double magnitude))
    chroma))

(defn stft-chroma
  "Extracts chroma vectors from STFT spectra."
  [spectrogram sample-rate fft-size min-frequency max-frequency]
  (mapv
    (fn [spectrum]
      (let [magnitudes (:magnitudes spectrum)
            chroma
            (reduce
              (fn [result bin]
                (let [frequency
                      (/ (* bin sample-rate) fft-size)]
                  (if (<= min-frequency frequency max-frequency)
                    (add-to-chroma
                      result
                      frequency
                      (nth magnitudes bin))
                    result)))
              (create-empty-chroma)
              (range (count magnitudes)))]

        {:frame-index (:frame-index spectrum)
         :time-seconds (:time-seconds spectrum)
         :values (normalize-chroma chroma)}))
    spectrogram))

(defn cqt-chroma
  "Extracts chroma vectors from CQT spectra."
  [cqt-result]
  (mapv
    (fn [spectrum]
      (let [frequencies (:frequencies spectrum)
            magnitudes (:magnitudes spectrum)
            chroma
            (reduce
              (fn [result bin]
                (add-to-chroma
                  result
                  (nth frequencies bin)
                  (nth magnitudes bin)))
              (create-empty-chroma)
              (range (count frequencies)))]

        {:frame-index (:frame-index spectrum)
         :time-seconds (:time-seconds spectrum)
         :values (normalize-chroma chroma)}))
    cqt-result))

(defn average-chroma
  "Calculates the average chroma profile across all frames."
  [chroma-frames]
  (if (empty? chroma-frames)
    (create-empty-chroma)
    (let [sum
          (reduce
            (fn [result frame]
              (mapv + result (:values frame)))
            (create-empty-chroma)
            chroma-frames)

          frame-count (count chroma-frames)]

      (normalize-chroma
        (mapv #(/ % frame-count) sum)))))

(defn rank-pitch-classes
  "Returns pitch classes sorted by their chroma strength."
  [chroma]
  (->> (map-indexed
         (fn [index value]
           {:pitch-class (nth pitch-classes index)
            :value value})
         chroma)
       (sort-by :value >)
       vec))
