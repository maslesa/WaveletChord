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