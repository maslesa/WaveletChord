(ns wavelet-chord.chroma-test
  (:require [clojure.test :refer :all]
            [wavelet-chord.chroma :as chroma]))

(deftest pitch-classes-test
  (testing "Contains twelve pitch classes"
    (is (= 12 (count chroma/pitch-classes))))

  (testing "Starts with C and ends with B"
    (is (= "C" (first chroma/pitch-classes)))
    (is (= "B" (last chroma/pitch-classes)))))

(deftest frequency-to-midi-test
  (testing "Converts A4 frequency to MIDI note 69"
    (is (= 69
           (chroma/frequency-to-midi 440.0))))

  (testing "Converts C4 frequency to MIDI note 60"
    (is (= 60
           (chroma/frequency-to-midi 261.625565))))

  (testing "Converts A5 frequency to MIDI note 81"
    (is (= 81
           (chroma/frequency-to-midi 880.0))))

  (testing "Returns nil for zero frequency"
    (is (nil?
          (chroma/frequency-to-midi 0.0))))

  (testing "Returns nil for negative frequency"
    (is (nil?
          (chroma/frequency-to-midi -440.0)))))

(deftest frequency-to-pitch-class-test
  (testing "Maps A4 to pitch class A"
    (is (= 9
           (chroma/frequency-to-pitch-class 440.0))))

  (testing "Maps C4 to pitch class C"
    (is (= 0
           (chroma/frequency-to-pitch-class 261.625565))))

  (testing "Maps C5 to pitch class C"
    (is (= 0
           (chroma/frequency-to-pitch-class 523.251131))))

  (testing "Maps B4 to pitch class B"
    (is (= 11
           (chroma/frequency-to-pitch-class 493.883301))))

  (testing "Returns nil for invalid frequency"
    (is (nil?
          (chroma/frequency-to-pitch-class 0.0)))))

(deftest normalize-chroma-test
  (testing "Normalizes chroma values so their sum equals one"
    (let [result
          (chroma/normalize-chroma
            [1.0 2.0 3.0])]
      (is (< (Math/abs
               (- 1.0 (reduce + result)))
             1.0e-9))
      (is (= [(/ 1.0 6.0)
              (/ 2.0 6.0)
              (/ 3.0 6.0)]
             result))))

  (testing "Preserves a zero chroma vector"
    (is (= [0.0 0.0 0.0]
           (chroma/normalize-chroma
             [0.0 0.0 0.0])))))

(deftest create-empty-chroma-test
  (testing "Creates a twelve-dimensional zero vector"
    (let [result (chroma/create-empty-chroma)]
      (is (= 12 (count result)))
      (is (every? zero? result)))))

(deftest add-to-chroma-test
  (testing "Adds magnitude to the correct pitch class"
    (let [result
          (chroma/add-to-chroma
            (chroma/create-empty-chroma)
            440.0
            5.0)]
      (is (= 5.0 (nth result 9)))
      (is (= 0.0 (nth result 0)))))

  (testing "Accumulates magnitudes for the same pitch class"
    (let [initial (chroma/create-empty-chroma)
          first-result (chroma/add-to-chroma initial 440.0 2.0)
          second-result (chroma/add-to-chroma first-result 880.0 3.0)]
      (is (= 5.0 (nth second-result 9)))))

  (testing "Ignores zero frequency"
    (let [initial (chroma/create-empty-chroma)
          result (chroma/add-to-chroma initial 0.0 5.0)]
      (is (= initial result)))))

(deftest stft-chroma-test
  (testing "Extracts chroma from an STFT spectrum"
    (let [magnitudes
          (assoc (vec (repeat 601 0.0)) 44 10.0)
          spectrogram
          [{:frame-index 0
            :time-seconds 0.0
            :magnitudes magnitudes}]
          result
          (chroma/stft-chroma
            spectrogram
            12000.0
            1200
            400.0
            500.0)
          values (:values (first result))]

      (is (= 1 (count result)))
      (is (= 12 (count values)))
      (is (= 1.0 (nth values 9)))
      (is (= 0 (:frame-index (first result))))
      (is (= 0.0 (:time-seconds (first result))))))

  (testing "Returns zero chroma when no bins are in the frequency range"
    (let [spectrogram
          [{:frame-index 0
            :time-seconds 0.0
            :magnitudes [0.0 0.0 0.0 0.0]}]
          result
          (chroma/stft-chroma
            spectrogram
            8000.0
            8
            1000.0
            2000.0)]
      (is (= (chroma/create-empty-chroma)
             (:values (first result))))))

  (testing "Returns an empty result for an empty spectrogram"
    (is (= []
           (chroma/stft-chroma
             []
             12000.0
             1200
             100.0
             1000.0)))))

(deftest cqt-chroma-test
  (testing "Extracts chroma from CQT bins"
    (let [cqt-result
          [{:frame-index 0
            :time-seconds 0.0
            :frequencies [440.0 523.251131]
            :magnitudes [1.0 2.0]}]
          result (chroma/cqt-chroma cqt-result)
          values (:values (first result))]

      (is (= 1 (count result)))
      (is (= 12 (count values)))

      ;; C receives 2/3, A receives 1/3.
      (is (< (Math/abs
               (- (/ 2.0 3.0) (nth values 0)))
             1.0e-9))
      (is (< (Math/abs
               (- (/ 1.0 3.0) (nth values 9)))
             1.0e-9))))

  (testing "Returns an empty result for empty CQT input"
    (is (= []
           (chroma/cqt-chroma [])))))

(deftest average-chroma-test
  (testing "Calculates the average chroma profile"
    (let [frames
          [{:values [1.0 0.0 0.0]}
           {:values [0.0 1.0 0.0]}]
          result (chroma/average-chroma frames)]
      (is (= [0.5 0.5 0.0] result))))

  (testing "Returns an empty chroma profile for no frames"
    (is (= (chroma/create-empty-chroma)
           (chroma/average-chroma [])))))

(deftest rank-pitch-classes-test
  (testing "Sorts pitch classes by descending strength"
    (let [profile
          [0.1 0.2 0.0 0.0 0.0 0.0
           0.0 0.0 0.0 0.5 0.1 0.1]
          result (chroma/rank-pitch-classes profile)]
      (is (= "A" (:pitch-class (first result))))
      (is (= 0.5 (:value (first result))))
      (is (= 12 (count result)))))

  (testing "Preserves all pitch classes"
    (let [result
          (chroma/rank-pitch-classes
            (chroma/create-empty-chroma))]
      (is (= (set chroma/pitch-classes)
             (set (map :pitch-class result)))))))

(def ^:private tolerance 1.0e-9)

(defn- approx=
  "True when two numbers differ by less than the tolerance."
  [expected actual]
  (< (Math/abs (- (double expected) (double actual))) tolerance))

(defn- approx-vec=
  "True when two vectors have the same size and approximately equal items."
  [expected actual]
  (and (= (count expected) (count actual))
       (every? true? (map approx= expected actual))))

(defn- semitone-frequency
  "Equal-tempered frequency located the given number of semitones above C4."
  [semitones]
  (* 261.625565 (Math/pow 2.0 (/ semitones 12.0))))

(deftest pitch-classes-additional-test
  (testing "Lists the pitch classes in chromatic order"
    (is (= ["C" "C#" "D" "D#" "E" "F"
            "F#" "G" "G#" "A" "A#" "B"]
           chroma/pitch-classes)))

  (testing "Contains no duplicate pitch classes"
    (is (= 12 (count (set chroma/pitch-classes)))))

  (testing "Contains only strings"
    (is (every? string? chroma/pitch-classes)))

  (testing "Has A at index 9, matching the pitch class of 440 Hz"
    (is (= "A" (nth chroma/pitch-classes 9)))
    (is (= "A" (nth chroma/pitch-classes
                    (chroma/frequency-to-pitch-class 440.0))))))

(deftest frequency-to-midi-additional-test
  (testing "Converts the lowest and highest piano notes"
    (is (= 21 (chroma/frequency-to-midi 27.5)))
    (is (= 108 (chroma/frequency-to-midi 4186.009))))

  (testing "Converts an A-sharp"
    (is (= 70 (chroma/frequency-to-midi 466.163762))))

  (testing "Accepts an integer frequency"
    (is (= 69 (chroma/frequency-to-midi 440))))

  (testing "Returns an integer"
    (is (integer? (chroma/frequency-to-midi 440.0))))

  (testing "Rounds slightly sharp frequencies down to the nearest note"
    (let [forty-cents-sharp (* 440.0 (Math/pow 2.0 (/ 0.4 12.0)))]
      (is (= 69 (chroma/frequency-to-midi forty-cents-sharp)))))

  (testing "Rounds slightly flat frequencies up to the nearest note"
    (let [forty-cents-flat (* 440.0 (Math/pow 2.0 (/ -0.4 12.0)))]
      (is (= 69 (chroma/frequency-to-midi forty-cents-flat)))))

  (testing "Rounds frequencies more than half a semitone away to the next note"
    (let [sixty-cents-sharp (* 440.0 (Math/pow 2.0 (/ 0.6 12.0)))
          sixty-cents-flat (* 440.0 (Math/pow 2.0 (/ -0.6 12.0)))]
      (is (= 70 (chroma/frequency-to-midi sixty-cents-sharp)))
      (is (= 68 (chroma/frequency-to-midi sixty-cents-flat)))))

  (testing "Doubling the frequency raises the note by one octave"
    (doseq [frequency [110.0 261.625565 329.627557 391.995436 440.0]]
      (is (= (+ 12 (chroma/frequency-to-midi frequency))
             (chroma/frequency-to-midi (* 2.0 frequency))))))

  (testing "Returns a note below zero for very low positive frequencies"
    (is (= -36 (chroma/frequency-to-midi 1.0))))

  (testing "Returns nil for a very small negative frequency"
    (is (nil? (chroma/frequency-to-midi -0.001)))))

(deftest frequency-to-pitch-class-additional-test
  (testing "Maps every octave of A to pitch class A"
    (doseq [frequency [55.0 110.0 220.0 440.0 880.0 1760.0]]
      (is (= 9 (chroma/frequency-to-pitch-class frequency)))))

  (testing "Maps the twelve semitones above C4 to pitch classes 0 to 11"
    (is (= (vec (range 12))
           (mapv (fn [semitones]
                   (chroma/frequency-to-pitch-class
                     (semitone-frequency semitones)))
                 (range 12)))))

  (testing "Maps frequencies slightly off pitch to the nearest pitch class"
    (is (= 9 (chroma/frequency-to-pitch-class 435.0)))
    (is (= 9 (chroma/frequency-to-pitch-class 445.0))))

  (testing "Always returns a value between 0 and 11 for positive frequencies"
    (doseq [frequency [1.0 5.0 20.0 100.0 1000.0 5000.0 20000.0]]
      (let [pitch-class (chroma/frequency-to-pitch-class frequency)]
        (is (<= 0 pitch-class 11)))))

  (testing "Returns nil for a negative frequency"
    (is (nil? (chroma/frequency-to-pitch-class -261.625565))))

  (testing "Can be used as an index into the pitch class names"
    (is (= "A" (nth chroma/pitch-classes
                    (chroma/frequency-to-pitch-class 440.0))))
    (is (= "C" (nth chroma/pitch-classes
                    (chroma/frequency-to-pitch-class 261.625565))))
    (is (= "G" (nth chroma/pitch-classes
                    (chroma/frequency-to-pitch-class 391.995436))))))

(deftest normalize-chroma-additional-test
  (testing "Keeps an already normalized vector unchanged"
    (is (= [0.25 0.25 0.5]
           (chroma/normalize-chroma [0.25 0.25 0.5]))))

  (testing "Accepts integer values"
    (is (approx-vec= [0.25 0.25 0.5]
                     (chroma/normalize-chroma [1 1 2]))))

  (testing "Produces the same result regardless of the overall scale"
    (is (approx-vec= (chroma/normalize-chroma [1.0 2.0 3.0])
                     (chroma/normalize-chroma [10.0 20.0 30.0]))))

  (testing "Preserves the ratios between the values"
    (let [result (chroma/normalize-chroma [1.0 2.0 4.0])]
      (is (approx= (* 2.0 (nth result 0)) (nth result 1)))
      (is (approx= (* 4.0 (nth result 0)) (nth result 2)))))

  (testing "Normalizes a single non-zero value to one"
    (is (= [1.0] (chroma/normalize-chroma [7.0]))))

  (testing "Normalizes a full twelve-dimensional vector"
    (let [result (chroma/normalize-chroma (vec (range 1 13)))]
      (is (= 12 (count result)))
      (is (approx= 1.0 (reduce + result)))))

  (testing "Returns an empty vector for empty input"
    (is (= [] (chroma/normalize-chroma []))))

  (testing "Always returns a vector"
    (is (vector? (chroma/normalize-chroma '(1.0 2.0 3.0))))
    (is (vector? (chroma/normalize-chroma '(0.0 0.0 0.0))))))

(deftest create-empty-chroma-additional-test
  (testing "Returns a vector"
    (is (vector? (chroma/create-empty-chroma))))

  (testing "Contains only double values"
    (is (every? #(instance? Double %) (chroma/create-empty-chroma))))

  (testing "Equals a vector of twelve 0.0 values"
    (is (= (vec (repeat 12 0.0))
           (chroma/create-empty-chroma))))

  (testing "Returns equal but independent values on every call"
    (let [first-chroma (chroma/create-empty-chroma)
          modified (assoc first-chroma 0 1.0)]
      (is (= (chroma/create-empty-chroma) first-chroma))
      (is (not= first-chroma modified))
      (is (= (chroma/create-empty-chroma)
             (chroma/create-empty-chroma))))))

(deftest add-to-chroma-additional-test
  (testing "Leaves all other pitch classes untouched"
    (let [result (chroma/add-to-chroma
                   (chroma/create-empty-chroma) 440.0 5.0)]
      (is (every? zero?
                  (keep-indexed (fn [index value]
                                  (when (not= index 9) value))
                                result)))))

  (testing "Keeps the vector length at twelve"
    (let [result (chroma/add-to-chroma
                   (chroma/create-empty-chroma) 440.0 5.0)]
      (is (= 12 (count result)))))

  (testing "Does not modify the input chroma"
    (let [initial (chroma/create-empty-chroma)]
      (chroma/add-to-chroma initial 440.0 5.0)
      (is (= (chroma/create-empty-chroma) initial))))

  (testing "Adds to a chroma that already has values"
    (let [initial (assoc (chroma/create-empty-chroma) 9 1.0)
          result (chroma/add-to-chroma initial 440.0 2.0)]
      (is (= 3.0 (nth result 9)))))

  (testing "Stores the magnitude as a double when given an integer"
    (let [result (chroma/add-to-chroma
                   (chroma/create-empty-chroma) 440.0 5)]
      (is (= 5.0 (nth result 9)))
      (is (instance? Double (nth result 9)))))

  (testing "Ignores negative frequencies"
    (let [initial (chroma/create-empty-chroma)]
      (is (= initial (chroma/add-to-chroma initial -440.0 5.0)))))

  (testing "Does not change the chroma for a zero magnitude"
    (let [initial (chroma/create-empty-chroma)]
      (is (= initial (chroma/add-to-chroma initial 440.0 0.0)))))

  (testing "Maps slightly detuned frequencies to the same pitch class"
    (let [result (-> (chroma/create-empty-chroma)
                     (chroma/add-to-chroma 439.0 1.0)
                     (chroma/add-to-chroma 441.0 1.0))]
      (is (= 2.0 (nth result 9)))))

  (testing "Adds different frequencies to different pitch classes"
    (let [result (-> (chroma/create-empty-chroma)
                     (chroma/add-to-chroma 261.625565 1.0)
                     (chroma/add-to-chroma 440.0 2.0))]
      (is (= 1.0 (nth result 0)))
      (is (= 2.0 (nth result 9)))
      (is (= 3.0 (reduce + result))))))

(deftest stft-chroma-additional-test
  (testing "Returns a vector with the expected keys"
    (let [spectrogram [{:frame-index 0
                        :time-seconds 0.0
                        :magnitudes (assoc (vec (repeat 601 0.0)) 44 10.0)}]
          result (chroma/stft-chroma spectrogram 12000.0 1200 400.0 500.0)]
      (is (vector? result))
      (is (= #{:frame-index :time-seconds :values}
             (set (keys (first result)))))))

  (testing "Ignores bins below the minimum frequency"
    ;; With these parameters every bin is 10 Hz wide, so bin 30 is 300 Hz.
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 30 100.0)
                         (assoc 44 10.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 400.0 500.0)
          values (:values (first result))]
      (is (= 1.0 (nth values 9)))
      (is (= 1.0 (reduce + values)))))

  (testing "Ignores bins above the maximum frequency"
    ;; Bin 60 is 600 Hz.
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 60 100.0)
                         (assoc 44 10.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 400.0 500.0)
          values (:values (first result))]
      (is (= 1.0 (nth values 9)))
      (is (= 1.0 (reduce + values)))))

  (testing "Includes bins located exactly on the frequency limits"
    (let [magnitudes (assoc (vec (repeat 601 0.0)) 44 10.0)
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 440.0 440.0)]
      (is (= 1.0 (nth (:values (first result)) 9)))))

  (testing "Ignores the DC bin even when the minimum frequency is zero"
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 0 50.0)
                         (assoc 44 10.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 0.0 500.0)
          values (:values (first result))]
      (is (= 1.0 (nth values 9)))
      (is (= 1.0 (reduce + values)))))

  (testing "Splits energy between pitch classes proportionally"
    ;; Bin 26 is 260 Hz (C), bin 44 is 440 Hz (A).
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 26 1.0)
                         (assoc 44 3.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 200.0 500.0)
          values (:values (first result))]
      (is (approx= 0.25 (nth values 0)))
      (is (approx= 0.75 (nth values 9)))
      (is (approx= 1.0 (reduce + values)))))

  (testing "Sums bins from different octaves into the same pitch class"
    ;; Bin 44 is 440 Hz (A), bin 88 is 880 Hz (A), bin 26 is 260 Hz (C).
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 26 2.0)
                         (assoc 44 1.0)
                         (assoc 88 1.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 200.0 1000.0)
          values (:values (first result))]
      (is (approx= 0.5 (nth values 0)))
      (is (approx= 0.5 (nth values 9)))))

  (testing "Processes several frames and keeps their metadata"
    (let [silent (vec (repeat 601 0.0))
          spectrogram [{:frame-index 0
                        :time-seconds 0.0
                        :magnitudes (assoc silent 44 10.0)}
                       {:frame-index 1
                        :time-seconds 0.5
                        :magnitudes (assoc silent 26 10.0)}
                       {:frame-index 2
                        :time-seconds 1.0
                        :magnitudes silent}]
          result (chroma/stft-chroma spectrogram 12000.0 1200 200.0 500.0)]
      (is (= 3 (count result)))
      (is (= [0 1 2] (mapv :frame-index result)))
      (is (= [0.0 0.5 1.0] (mapv :time-seconds result)))
      (is (= 1.0 (nth (:values (nth result 0)) 9)))
      (is (= 1.0 (nth (:values (nth result 1)) 0)))
      (is (= (chroma/create-empty-chroma)
             (:values (nth result 2))))))

  (testing "Returns normalized values for every frame"
    (let [magnitudes (-> (vec (repeat 601 0.0))
                         (assoc 26 2.0)
                         (assoc 44 5.0)
                         (assoc 66 1.0))
          result (chroma/stft-chroma
                   [{:frame-index 0 :time-seconds 0.0 :magnitudes magnitudes}]
                   12000.0 1200 200.0 1000.0)]
      (is (approx= 1.0 (reduce + (:values (first result))))))))

(deftest cqt-chroma-additional-test
  (testing "Returns a vector with the expected keys"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0]
                     :magnitudes [1.0]}])]
      (is (vector? result))
      (is (= #{:frame-index :time-seconds :values}
             (set (keys (first result)))))))

  (testing "Normalizes the values so they sum to one"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [261.625565 329.627557 391.995436]
                     :magnitudes [4.0 2.0 1.0]}])]
      (is (approx= 1.0 (reduce + (:values (first result)))))))

  (testing "Ignores bins with a non-positive frequency"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [0.0 -100.0 440.0]
                     :magnitudes [100.0 100.0 1.0]}])
          values (:values (first result))]
      (is (= 1.0 (nth values 9)))
      (is (approx= 1.0 (reduce + values)))))

  (testing "Sums bins from different octaves into the same pitch class"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0 880.0 261.625565]
                     :magnitudes [1.0 2.0 1.0]}])
          values (:values (first result))]
      (is (approx= 0.75 (nth values 9)))
      (is (approx= 0.25 (nth values 0)))))

  (testing "Returns a zero chroma when all magnitudes are zero"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0 523.251131]
                     :magnitudes [0.0 0.0]}])]
      (is (= (chroma/create-empty-chroma)
             (:values (first result))))))

  (testing "Returns a zero chroma for a frame without bins"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies []
                     :magnitudes []}])]
      (is (= (chroma/create-empty-chroma)
             (:values (first result))))))

  (testing "Processes several frames and keeps their metadata"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0]
                     :magnitudes [1.0]}
                    {:frame-index 1
                     :time-seconds 0.25
                     :frequencies [261.625565]
                     :magnitudes [1.0]}])]
      (is (= 2 (count result)))
      (is (= [0 1] (mapv :frame-index result)))
      (is (= [0.0 0.25] (mapv :time-seconds result)))
      (is (= 1.0 (nth (:values (nth result 0)) 9)))
      (is (= 1.0 (nth (:values (nth result 1)) 0)))))

  (testing "Maps a C major triad to C, E and G"
    (let [result (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [261.625565 329.627557 391.995436]
                     :magnitudes [1.0 1.0 1.0]}])
          values (:values (first result))]
      (is (approx= (/ 1.0 3.0) (nth values 0)))
      (is (approx= (/ 1.0 3.0) (nth values 4)))
      (is (approx= (/ 1.0 3.0) (nth values 7))))))

(deftest wavelet-chroma-test
  (testing "Extracts chroma from wavelet bins"
    (let [wavelet-result
          [{:frame-index 0
            :time-seconds 0.0
            :frequencies [440.0 523.251131]
            :magnitudes [1.0 2.0]}]
          result (chroma/wavelet-chroma wavelet-result)
          values (:values (first result))]
      (is (= 1 (count result)))
      (is (= 12 (count values)))

      ;; C receives 2/3, A receives 1/3.
      (is (approx= (/ 2.0 3.0) (nth values 0)))
      (is (approx= (/ 1.0 3.0) (nth values 9)))))

  (testing "Returns an empty result for empty wavelet input"
    (is (= [] (chroma/wavelet-chroma []))))

  (testing "Returns a vector with the expected keys"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 3
                     :time-seconds 1.5
                     :frequencies [440.0]
                     :magnitudes [1.0]}])]
      (is (vector? result))
      (is (= #{:frame-index :time-seconds :values}
             (set (keys (first result)))))))

  (testing "Keeps the frame index and time of each frame"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0]
                     :magnitudes [1.0]}
                    {:frame-index 1
                     :time-seconds 0.1
                     :frequencies [261.625565]
                     :magnitudes [1.0]}
                    {:frame-index 2
                     :time-seconds 0.2
                     :frequencies [391.995436]
                     :magnitudes [1.0]}])]
      (is (= 3 (count result)))
      (is (= [0 1 2] (mapv :frame-index result)))
      (is (= [0.0 0.1 0.2] (mapv :time-seconds result)))))

  (testing "Assigns each frame to its own pitch class"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0]
                     :magnitudes [1.0]}
                    {:frame-index 1
                     :time-seconds 0.1
                     :frequencies [261.625565]
                     :magnitudes [1.0]}])]
      (is (= 1.0 (nth (:values (nth result 0)) 9)))
      (is (= 1.0 (nth (:values (nth result 1)) 0)))))

  (testing "Normalizes the values so they sum to one"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [261.625565 329.627557 391.995436 440.0]
                     :magnitudes [4.0 3.0 2.0 1.0]}])]
      (is (approx= 1.0 (reduce + (:values (first result)))))))

  (testing "Ignores bins with a non-positive frequency"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [0.0 -50.0 440.0]
                     :magnitudes [100.0 100.0 1.0]}])
          values (:values (first result))]
      (is (= 1.0 (nth values 9)))
      (is (approx= 1.0 (reduce + values)))))

  (testing "Sums bins from different octaves into the same pitch class"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [220.0 440.0 880.0 261.625565]
                     :magnitudes [1.0 1.0 1.0 1.0]}])
          values (:values (first result))]
      (is (approx= 0.75 (nth values 9)))
      (is (approx= 0.25 (nth values 0)))))

  (testing "Maps slightly detuned frequencies to the nearest pitch class"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [435.0 445.0]
                     :magnitudes [1.0 1.0]}])]
      (is (= 1.0 (nth (:values (first result)) 9)))))

  (testing "Returns a zero chroma when all magnitudes are zero"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0 523.251131]
                     :magnitudes [0.0 0.0]}])]
      (is (= (chroma/create-empty-chroma)
             (:values (first result))))))

  (testing "Returns a zero chroma for a frame without bins"
    (let [result (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies []
                     :magnitudes []}])]
      (is (= (chroma/create-empty-chroma)
             (:values (first result))))))

  (testing "Does not depend on the order of the bins"
    (let [ascending (chroma/wavelet-chroma
                      [{:frame-index 0
                        :time-seconds 0.0
                        :frequencies [261.625565 329.627557 440.0]
                        :magnitudes [1.0 2.0 3.0]}])
          descending (chroma/wavelet-chroma
                       [{:frame-index 0
                         :time-seconds 0.0
                         :frequencies [440.0 329.627557 261.625565]
                         :magnitudes [3.0 2.0 1.0]}])]
      (is (approx-vec= (:values (first ascending))
                       (:values (first descending))))))

  (testing "Produces the same result as the CQT extraction for the same input"
    (let [input [{:frame-index 0
                  :time-seconds 0.0
                  :frequencies [261.625565 329.627557 391.995436]
                  :magnitudes [1.0 2.0 3.0]}
                 {:frame-index 1
                  :time-seconds 0.1
                  :frequencies [440.0 880.0]
                  :magnitudes [5.0 1.0]}]]
      (is (= (chroma/cqt-chroma input)
             (chroma/wavelet-chroma input))))))

(deftest average-chroma-additional-test
  (testing "Returns the values of a single frame"
    (let [result (chroma/average-chroma [{:values [0.2 0.3 0.5]}])]
      (is (approx-vec= [0.2 0.3 0.5] result))))

  (testing "Averages several frames"
    (let [result (chroma/average-chroma
                   [{:values [0.8 0.2 0.0]}
                    {:values [0.4 0.6 0.0]}])]
      (is (approx-vec= [0.6 0.4 0.0] result))))

  (testing "Distributes equally between three equally strong frames"
    (let [result (chroma/average-chroma
                   [{:values [1.0 0.0 0.0]}
                    {:values [0.0 1.0 0.0]}
                    {:values [0.0 0.0 1.0]}])]
      (is (approx-vec= [(/ 1.0 3.0) (/ 1.0 3.0) (/ 1.0 3.0)] result))
      (is (approx= 1.0 (reduce + result)))))

  (testing "Returns a normalized profile"
    (let [result (chroma/average-chroma
                   [{:values [0.5 0.5 0.0]}
                    {:values [0.0 0.25 0.75]}
                    {:values [0.1 0.1 0.8]}])]
      (is (approx= 1.0 (reduce + result)))))

  (testing "Does not depend on the order of the frames"
    (let [frames [{:values [0.7 0.3 0.0]}
                  {:values [0.1 0.1 0.8]}
                  {:values [0.0 0.5 0.5]}]]
      (is (approx-vec= (chroma/average-chroma frames)
                       (chroma/average-chroma (reverse frames))))))

  (testing "Returns a zero profile when every frame is zero"
    (let [zero-frame {:values (chroma/create-empty-chroma)}
          result (chroma/average-chroma [zero-frame zero-frame])]
      (is (= (chroma/create-empty-chroma) result))))

  (testing "Averages full twelve-dimensional frames"
    (let [a-frame {:values (assoc (chroma/create-empty-chroma) 9 1.0)}
          c-frame {:values (assoc (chroma/create-empty-chroma) 0 1.0)}
          result (chroma/average-chroma [a-frame c-frame])]
      (is (= 12 (count result)))
      (is (approx= 0.5 (nth result 0)))
      (is (approx= 0.5 (nth result 9)))))

  (testing "Returns a vector"
    (is (vector? (chroma/average-chroma [{:values [1.0 0.0 0.0]}])))
    (is (vector? (chroma/average-chroma [])))))

(deftest rank-pitch-classes-additional-test
  (testing "Returns a vector of maps with pitch-class and value keys"
    (let [result (chroma/rank-pitch-classes (chroma/create-empty-chroma))]
      (is (vector? result))
      (is (every? #(= #{:pitch-class :value} (set (keys %))) result))))

  (testing "Returns the values in descending order"
    (let [profile [0.05 0.0 0.1 0.0 0.2 0.0
                   0.0 0.4 0.0 0.15 0.0 0.1]
          result (chroma/rank-pitch-classes profile)]
      (is (apply >= (map :value result)))))

  (testing "Ranks the strongest pitch classes first"
    (let [profile (-> (chroma/create-empty-chroma)
                      (assoc 0 0.3)
                      (assoc 4 0.2)
                      (assoc 7 0.5))
          result (chroma/rank-pitch-classes profile)]
      (is (= ["G" "C" "E"]
             (mapv :pitch-class (take 3 result))))
      (is (= [0.5 0.3 0.2]
             (mapv :value (take 3 result))))))

  (testing "Places the weakest pitch class last"
    (let [profile [0.1 0.05 0.1 0.1 0.1 0.1
                   0.1 0.1 0.1 0.1 0.1 0.05]
          result (chroma/rank-pitch-classes profile)]
      (is (= 0.05 (:value (last result))))))

  (testing "Pairs every value with the correct pitch class name"
    (let [profile (vec (range 12))
          result (chroma/rank-pitch-classes profile)
          by-name (into {} (map (juxt :pitch-class :value) result))]
      (doseq [index (range 12)]
        (is (= index (get by-name (nth chroma/pitch-classes index)))))))

  (testing "Uses sharp names for black keys"
    (let [profile (assoc (chroma/create-empty-chroma) 1 1.0)
          result (chroma/rank-pitch-classes profile)]
      (is (= "C#" (:pitch-class (first result))))))

  (testing "Keeps the chromatic order for equal values"
    (let [result (chroma/rank-pitch-classes (chroma/create-empty-chroma))]
      (is (= chroma/pitch-classes
             (mapv :pitch-class result)))))

  (testing "Keeps the chromatic order for tied strongest pitch classes"
    (let [profile (-> (chroma/create-empty-chroma)
                      (assoc 2 0.4)
                      (assoc 0 0.4))
          result (chroma/rank-pitch-classes profile)]
      (is (= ["C" "D"]
             (mapv :pitch-class (take 2 result))))))

  (testing "Does not drop or duplicate any pitch class"
    (let [profile [0.3 0.1 0.0 0.2 0.0 0.0
                   0.1 0.0 0.1 0.2 0.0 0.0]
          result (chroma/rank-pitch-classes profile)]
      (is (= 12 (count result)))
      (is (= (set chroma/pitch-classes)
             (set (map :pitch-class result)))))))

(deftest chroma-pipeline-test
  (testing "A pure A4 tone is ranked as A by the CQT pipeline"
    (let [frames (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0]
                     :magnitudes [1.0]}
                    {:frame-index 1
                     :time-seconds 0.1
                     :frequencies [440.0]
                     :magnitudes [1.0]}])
          ranking (chroma/rank-pitch-classes
                    (chroma/average-chroma frames))]
      (is (= "A" (:pitch-class (first ranking))))
      (is (approx= 1.0 (:value (first ranking))))))

  (testing "A C major triad is ranked as C, E and G by the CQT pipeline"
    (let [frames (chroma/cqt-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [261.625565 329.627557 391.995436]
                     :magnitudes [1.0 1.0 1.0]}])
          ranking (chroma/rank-pitch-classes
                    (chroma/average-chroma frames))]
      (is (= #{"C" "E" "G"}
             (set (map :pitch-class (take 3 ranking)))))
      (is (approx= (/ 1.0 3.0) (:value (first ranking))))))

  (testing "An A minor triad is ranked as A, C and E by the wavelet pipeline"
    (let [frames (chroma/wavelet-chroma
                   [{:frame-index 0
                     :time-seconds 0.0
                     :frequencies [440.0 523.251131 659.255114]
                     :magnitudes [1.0 1.0 1.0]}])
          ranking (chroma/rank-pitch-classes
                    (chroma/average-chroma frames))]
      (is (= #{"A" "C" "E"}
             (set (map :pitch-class (take 3 ranking)))))))

  (testing "A steady STFT tone is ranked as A across several frames"
    (let [magnitudes (assoc (vec (repeat 601 0.0)) 44 10.0)
          spectrogram (mapv (fn [index]
                              {:frame-index index
                               :time-seconds (* 0.1 index)
                               :magnitudes magnitudes})
                            (range 4))
          frames (chroma/stft-chroma spectrogram 12000.0 1200 100.0 1000.0)
          ranking (chroma/rank-pitch-classes
                    (chroma/average-chroma frames))]
      (is (= 4 (count frames)))
      (is (= "A" (:pitch-class (first ranking))))
      (is (approx= 1.0 (:value (first ranking)))))))