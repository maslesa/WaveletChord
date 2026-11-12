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