(ns wavelet-chord.signal-test
  (:require [clojure.test :refer :all]
            [wavelet-chord.signal :as signal]))

(deftest normalize-signal-test
  (testing "Normalizes signal using its peak amplitude"
    (is (= [0.5 -1.0 0.25]
           (signal/normalize-signal [0.5 -1.0 0.25]))))

  (testing "Normalizes signal with different amplitudes"
    (is (= [0.5 -1.0 0.0 1.0]
           (signal/normalize-signal [1.0 -2.0 0.0 2.0]))))

  (testing "Preserves a silent signal"
    (is (= [0.0 0.0 0.0]
           (signal/normalize-signal [0.0 0.0 0.0])))))

(deftest mean-amplitude-test
  (testing "Calculates mean absolute amplitude"
    (is (= 0.5
           (signal/mean-amplitude [1.0 -0.5 0.0]))))

  (testing "Returns zero for an empty signal"
    (is (= 0.0
           (signal/mean-amplitude []))))

  (testing "Returns zero for a silent signal"
    (is (= 0.0
           (signal/mean-amplitude [0.0 0.0 0.0])))))

(deftest rms-amplitude-test
  (testing "Calculates RMS amplitude"
    (is (< (Math/abs
             (- (signal/rms-amplitude [1.0 -1.0])
                1.0))
           1.0e-9)))

  (testing "Returns zero for an empty signal"
    (is (= 0.0
           (signal/rms-amplitude []))))

  (testing "Returns zero for a silent signal"
    (is (= 0.0
           (signal/rms-amplitude [0.0 0.0 0.0])))))

(deftest split-into-frames-test
  (testing "Splits signal into frames using the specified hop size"
    (is (= [[1 2 3 4]
            [3 4 5 6]
            [5 6 7 8]]
           (signal/split-into-frames
             [1 2 3 4 5 6 7 8]
             4
             2))))

  (testing "Returns one frame when signal matches frame size"
    (is (= [[1 2 3 4]]
           (signal/split-into-frames
             [1 2 3 4]
             4
             2))))

  (testing "Returns no frames when signal is shorter than frame size"
    (is (= []
           (signal/split-into-frames
             [1 2 3]
             4
             2))))

  (testing "Throws when frame size is zero"
    (is (thrown? clojure.lang.ExceptionInfo
                 (signal/split-into-frames [1 2 3] 0 1))))

  (testing "Throws when hop size is zero"
    (is (thrown? clojure.lang.ExceptionInfo
                 (signal/split-into-frames [1 2 3] 2 0)))))

(deftest hann-window-test
  (testing "Creates a Hann window of the requested size"
    (is (= 4
           (count (signal/hann-window 4)))))

  (testing "Hann window starts and ends at zero"
    (let [window (signal/hann-window 8)]
      (is (< (Math/abs (first window)) 1.0e-9))
      (is (< (Math/abs (last window)) 1.0e-9))))

  (testing "Hann window values are within the expected range"
    (let [window (signal/hann-window 16)]
      (is (every? #(<= 0.0 % 1.0) window))))

  (testing "Throws when frame size is too small"
    (is (thrown? clojure.lang.ExceptionInfo
                 (signal/hann-window 1)))))

(deftest apply-window-test
  (testing "Multiplies frame samples by window values"
    (is (= [0.0 1.0 1.5 0.0]
           (signal/apply-window
             [1.0 2.0 3.0 4.0]
             [0.0 0.5 0.5 0.0]))))

  (testing "Throws when frame and window sizes differ"
    (is (thrown? clojure.lang.ExceptionInfo
                 (signal/apply-window
                   [1.0 2.0]
                   [1.0])))))

(deftest fft-magnitudes-test
  (testing "Returns the expected number of FFT magnitudes"
    (is (= 5
           (count (signal/fft-magnitudes
                    [1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0])))))

  (testing "Calculates the DC component of a constant signal"
    (let [magnitudes
          (signal/fft-magnitudes
            [1.0 1.0 1.0 1.0])]
      (is (< (Math/abs (- 4.0 (first magnitudes)))
             1.0e-9))))

  (testing "Detects the expected frequency bin"
    (let [frame [1.0 0.0 -1.0 0.0]
          magnitudes (signal/fft-magnitudes frame)]
      (is (< (Math/abs (- 2.0 (nth magnitudes 1)))
             1.0e-9))))

  (testing "Throws for an empty frame"
    (is (thrown? Exception
                 (signal/fft-magnitudes [])))))

(deftest frequency-for-bin-test
  (testing "Calculates frequency for an FFT bin"
    (is (= 1000.0
           (signal/frequency-for-bin 1 8000.0 8))))

  (testing "Returns zero for the DC bin"
    (is (= 0.0
           (signal/frequency-for-bin 0 48000.0 8192)))))

(deftest stft-test
  (testing "Calculates STFT for multiple frames"
    (let [frames [[1.0 0.0 -1.0 0.0]
                  [0.0 1.0 0.0 -1.0]]
          window [1.0 1.0 1.0 1.0]
          result (signal/stft frames 8000.0 2 window)]

      (is (= 2 (count result)))

      (is (= 0 (:frame-index (first result))))
      (is (= 1 (:frame-index (second result))))

      (is (= 0.0 (:time-seconds (first result))))
      (is (= 0.00025 (:time-seconds (second result))))

      (is (= 3
             (count (:magnitudes (first result)))))))

  (testing "Returns an empty result for no frames"
    (is (= []
           (signal/stft [] 8000.0 2 [1.0 1.0 1.0 1.0])))))

(deftest load-audio-test
  (testing "Throws when the audio file does not exist"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"Audio file not found"
          (signal/load-audio
            "file-that-does-not-exist.wav")))))
