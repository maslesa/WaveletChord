(ns wavelet-chord.cqt-test
  (:require [clojure.test :refer :all]
            [wavelet-chord.cqt :as cqt]))

(deftest cqt-frequency-test
  (testing "Calculates the minimum CQT frequency"
    (is (< (Math/abs
             (- 440.0
                (cqt/cqt-frequency 0 440.0 12)))
           1.0e-9)))

  (testing "Calculates the frequency one octave above"
    (is (< (Math/abs
             (- 880.0
                (cqt/cqt-frequency 12 440.0 12)))
           1.0e-9)))

  (testing "Calculates the frequency one semitone above"
    (is (< (Math/abs
             (- (* 440.0 (Math/pow 2.0 (/ 1.0 12.0)))
                (cqt/cqt-frequency 1 440.0 12)))
           1.0e-9))))

(deftest cqt-kernel-test
  (testing "Creates a kernel with the expected structure"
    (let [kernel (cqt/cqt-kernel 440.0 8000.0 1024 10.0)]
      (is (= 440.0 (:frequency kernel)))
      (is (pos? (:kernel-size kernel)))
      (is (= (:kernel-size kernel)
             (alength ^doubles (:real kernel))))
      (is (= (:kernel-size kernel)
             (alength ^doubles (:imaginary kernel))))
      (is (pos? (:normalization kernel)))))

  (testing "Throws when kernel is larger than frame"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"CQT kernel is larger than the frame"
          (cqt/cqt-kernel 20.0 8000.0 128 10.0)))))

(deftest create-cqt-kernels-test
  (testing "Creates the requested number of kernels"
    (let [kernels
          (cqt/create-cqt-kernels
            8000.0
            1024
            440.0
            12
            12)]
      (is (= 12 (count kernels)))))

  (testing "Kernel frequencies are in ascending order"
    (let [kernels
          (cqt/create-cqt-kernels
            8000.0
            1024
            440.0
            12
            12)
          frequencies (mapv :frequency kernels)]
      (is (= (vec (sort frequencies))
             frequencies))))

  (testing "First kernel starts at the minimum frequency"
    (let [kernels
          (cqt/create-cqt-kernels
            8000.0
            1024
            440.0
            12
            12)]
      (is (< (Math/abs
               (- 440.0 (:frequency (first kernels))))
             1.0e-9)))))

(deftest calculate-cqt-bin-test
  (let [kernel (cqt/cqt-kernel 440.0 8000.0 1024 10.0)]

    (testing "Returns zero magnitude for a silent frame"
      (is (= 0.0
             (cqt/calculate-cqt-bin
               (vec (repeat 1024 0.0))
               kernel))))

    (testing "Returns a non-negative magnitude"
      (let [frame
            (mapv
              (fn [n]
                (Math/sin
                  (/ (* 2.0 Math/PI 440.0 n)
                     8000.0)))
              (range 1024))
            magnitude (cqt/calculate-cqt-bin frame kernel)]
        (is (number? magnitude))
        (is (not (neg? magnitude)))))))

(deftest cqt-test
  (testing "Calculates CQT for multiple frames"
    (let [frames
          [(vec (repeat 1024 0.0))
           (mapv
             (fn [n]
               (Math/sin
                 (/ (* 2.0 Math/PI 440.0 n)
                    8000.0)))
             (range 1024))]
          result
          (cqt/cqt
            frames
            8000.0
            256
            1024
            440.0
            12
            12)]

      (is (= 2 (count result)))
      (is (= 12 (count (:frequencies (first result)))))
      (is (= 12 (count (:magnitudes (first result)))))

      (is (= 0 (:frame-index (first result))))
      (is (= 1 (:frame-index (second result))))

      (is (= 0.0 (:time-seconds (first result))))
      (is (= 0.032 (:time-seconds (second result))))

      (is (every? zero? (:magnitudes (first result))))))

  (testing "Returns an empty result for no frames"
    (is (= []
           (cqt/cqt
             []
             8000.0
             256
             1024
             440.0
             12
             12)))))

(deftest cqt-frequency-additional-test
  (testing "Calculates frequencies below the minimum for negative bins"
    (is (< (Math/abs
             (- 220.0
                (cqt/cqt-frequency -12 440.0 12)))
           1.0e-9)))

  (testing "Calculates the frequency one octave above with 24 bins per octave"
    (is (< (Math/abs
             (- 880.0
                (cqt/cqt-frequency 24 440.0 24)))
           1.0e-9)))

  (testing "Calculates the frequency of a quarter tone with 24 bins per octave"
    (is (< (Math/abs
             (- (* 440.0 (Math/pow 2.0 (/ 1.0 24.0)))
                (cqt/cqt-frequency 1 440.0 24)))
           1.0e-9)))

  (testing "Calculates A4 from C4 nine semitones above"
    (is (< (Math/abs
             (- 440.0
                (cqt/cqt-frequency 9 261.625565 12)))
           1.0e-3)))

  (testing "Scales linearly with the minimum frequency"
    (is (< (Math/abs
             (- (* 2.0 (cqt/cqt-frequency 5 220.0 12))
                (cqt/cqt-frequency 5 440.0 12)))
           1.0e-9)))

  (testing "Keeps a constant ratio between consecutive bins"
    (let [ratio (Math/pow 2.0 (/ 1.0 12.0))]
      (is (every?
            true?
            (map (fn [bin]
                   (< (Math/abs
                        (- ratio
                           (/ (cqt/cqt-frequency (inc bin) 440.0 12)
                              (cqt/cqt-frequency bin 440.0 12))))
                      1.0e-9))
                 (range 24))))))

  (testing "Increases strictly with the bin index"
    (let [frequencies (map #(cqt/cqt-frequency % 440.0 12) (range 36))]
      (is (apply < frequencies))))

  (testing "Returns a double for an integer minimum frequency"
    (is (instance? Double (cqt/cqt-frequency 12 440 12)))))

(deftest cqt-kernel-additional-test
  (let [kernel (cqt/cqt-kernel 440.0 8000.0 1024 10.0)
        size (:kernel-size kernel)
        ^doubles real (:real kernel)
        ^doubles imaginary (:imaginary kernel)
        hann (fn [n]
               (* 0.5
                  (- 1.0
                     (Math/cos
                       (/ (* 2.0 Math/PI n)
                          (dec size))))))]

    (testing "Contains exactly the expected keys"
      (is (= #{:frequency :kernel-size :real :imaginary :normalization}
             (set (keys kernel)))))

    (testing "Calculates the kernel size from the Q factor and the frequency"
      (is (= 182 size)))

    (testing "Returns an integer kernel size"
      (is (integer? size)))

    (testing "Stores the kernel as arrays of doubles"
      (is (= (Class/forName "[D") (class (:real kernel))))
      (is (= (Class/forName "[D") (class (:imaginary kernel)))))

    (testing "Starts and ends the kernel at zero"
      (is (= 0.0 (aget real 0)))
      (is (= 0.0 (aget imaginary 0)))
      (is (< (Math/abs (aget real (dec size))) 1.0e-9))
      (is (< (Math/abs (aget imaginary (dec size))) 1.0e-9)))

    (testing "Applies a Hann window to a complex sinusoid"
      (is (every?
            true?
            (map (fn [n]
                   (let [angle (/ (* 2.0 Math/PI 440.0 n) 8000.0)]
                     (and (< (Math/abs
                               (- (* (hann n) (Math/cos angle))
                                  (aget real n)))
                             1.0e-9)
                          (< (Math/abs
                               (- (* (hann n) (Math/sin angle))
                                  (aget imaginary n)))
                             1.0e-9))))
                 (range size)))))

    (testing "Has an envelope that follows the Hann window"
      (is (every?
            true?
            (map (fn [n]
                   (let [envelope (Math/sqrt
                                    (+ (* (aget real n) (aget real n))
                                       (* (aget imaginary n)
                                          (aget imaginary n))))]
                     (< (Math/abs (- (hann n) envelope)) 1.0e-9)))
                 (range size)))))

    (testing "Has a symmetric envelope"
      (is (every?
            true?
            (map (fn [n]
                   (let [envelope
                         (fn [i]
                           (Math/sqrt
                             (+ (* (aget real i) (aget real i))
                                (* (aget imaginary i) (aget imaginary i)))))]
                     (< (Math/abs
                          (- (envelope n)
                             (envelope (- (dec size) n))))
                        1.0e-9)))
                 (range size)))))

    (testing "Has an envelope that never exceeds one and peaks near one"
      (let [envelopes (map (fn [n]
                             (Math/sqrt
                               (+ (* (aget real n) (aget real n))
                                  (* (aget imaginary n) (aget imaginary n)))))
                           (range size))]
        (is (<= (apply max envelopes) (+ 1.0 1.0e-9)))
        (is (> (apply max envelopes) 0.99))))

    (testing "Normalizes by the sum of the window"
      (let [window-sum (reduce + (map hann (range size)))]
        (is (< (Math/abs
                 (- (/ 2.0 window-sum)
                    (:normalization kernel)))
               1.0e-9))))

    (testing "Does not depend on the frame size"
      (let [other (cqt/cqt-kernel 440.0 8000.0 4096 10.0)]
        (is (= size (:kernel-size other)))
        (is (= (vec real) (vec (:real other))))
        (is (= (vec imaginary) (vec (:imaginary other))))
        (is (= (:normalization kernel) (:normalization other)))))

    (testing "Creates a shorter kernel for a higher frequency"
      (is (= 91
             (:kernel-size
               (cqt/cqt-kernel 880.0 8000.0 1024 10.0)))))

    (testing "Creates a longer kernel for a larger Q factor"
      (is (= 364
             (:kernel-size
               (cqt/cqt-kernel 440.0 8000.0 1024 20.0)))))

    (testing "Creates a longer kernel for a higher sample rate"
      (is (= 364
             (:kernel-size
               (cqt/cqt-kernel 440.0 16000.0 1024 10.0)))))

    (testing "Accepts a kernel that is exactly as large as the frame"
      (is (= 800
             (:kernel-size
               (cqt/cqt-kernel 100.0 8000.0 800 10.0)))))

    (testing "Throws when the kernel is larger than the frame by one sample"
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"CQT kernel is larger than the frame"
            (cqt/cqt-kernel 100.0 8000.0 799 10.0))))

    (testing "Reports the details of an oversized kernel"
      (let [data (try
                   (cqt/cqt-kernel 100.0 8000.0 799 10.0)
                   (catch clojure.lang.ExceptionInfo e
                     (ex-data e)))]
        (is (= {:frequency 100.0
                :kernel-size 800
                :frame-size 799}
               data))))))

(deftest create-cqt-kernels-additional-test
  (testing "Returns a vector of kernel maps"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)]
      (is (vector? kernels))
      (is (every? map? kernels))
      (is (every? #(= #{:frequency :kernel-size :real :imaginary
                        :normalization}
                      (set (keys %)))
                  kernels))))

  (testing "Kernel frequencies match the CQT bin frequencies"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)]
      (is (= (mapv #(cqt/cqt-frequency % 440.0 12) (range 12))
             (mapv :frequency kernels)))))

  (testing "Kernel sizes decrease as the frequency increases"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)
          sizes (mapv :kernel-size kernels)]
      (is (apply > sizes))))

  (testing "Kernels keep a constant Q factor"
    (let [q-factor (/ 1.0 (- (Math/pow 2.0 (/ 1.0 12.0)) 1.0))
          kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)]
      (is (every?
            true?
            (map (fn [kernel]
                   (let [frequency (:frequency kernel)
                         ratio (/ (* frequency (:kernel-size kernel))
                                  8000.0)]
                     (and (>= ratio (- q-factor 1.0e-9))
                          (< ratio (+ q-factor (/ frequency 8000.0))))))
                 kernels)))))

  (testing "Creates a single kernel at the minimum frequency"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 1)]
      (is (= 1 (count kernels)))
      (is (= 440.0 (:frequency (first kernels))))))

  (testing "Returns no kernels when zero bins are requested"
    (is (= [] (cqt/create-cqt-kernels 8000.0 1024 440.0 12 0))))

  (testing "Covers more than one octave"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 24)
          frequencies (mapv :frequency kernels)]
      (is (= 24 (count kernels)))
      (is (< (Math/abs (- 880.0 (nth frequencies 12))) 1.0e-9))
      (is (< (Math/abs
               (- (* 440.0 (Math/pow 2.0 (/ 23.0 12.0)))
                  (last frequencies)))
             1.0e-9))))

  (testing "Creates longer kernels for a higher resolution"
    (let [coarse (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)
          fine (cqt/create-cqt-kernels 8000.0 1024 440.0 24 24)]
      (is (= 24 (count fine)))
      (is (> (:kernel-size (first fine))
             (:kernel-size (first coarse))))))

  (testing "Supports 36 bins per octave"
    (let [kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 36 36)]
      (is (= 36 (count kernels)))
      (is (< (Math/abs
               (- (* 440.0 (Math/pow 2.0 (/ 35.0 36.0)))
                  (:frequency (last kernels))))
             1.0e-9))))

  (testing "Throws when the lowest kernel does not fit in the frame"
    (is (thrown-with-msg?
          clojure.lang.ExceptionInfo
          #"CQT kernel is larger than the frame"
          (cqt/create-cqt-kernels 8000.0 128 20.0 12 12)))))

(deftest calculate-cqt-bin-additional-test
  (let [kernel (cqt/cqt-kernel 440.0 8000.0 1024 10.0)
        sine (fn [frequency size amplitude]
               (mapv
                 (fn [n]
                   (* amplitude
                      (Math/sin
                        (/ (* 2.0 Math/PI frequency n)
                           8000.0))))
                 (range size)))
        cosine (fn [frequency size amplitude]
                 (mapv
                   (fn [n]
                     (* amplitude
                        (Math/cos
                          (/ (* 2.0 Math/PI frequency n)
                             8000.0))))
                   (range size)))]

    (testing "Returns a double"
      (is (instance?
            Double
            (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel))))

    (testing "Returns the amplitude of a sinusoid at the kernel frequency"
      (is (< (Math/abs
               (- 1.0
                  (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel)))
             1.0e-3)))

    (testing "Scales with the amplitude of the signal"
      (is (< (Math/abs
               (- 0.5
                  (cqt/calculate-cqt-bin (sine 440.0 1024 0.5) kernel)))
             1.0e-3))
      (is (< (Math/abs
               (- 3.0
                  (cqt/calculate-cqt-bin (sine 440.0 1024 3.0) kernel)))
             1.0e-3)))

    (testing "Doubles the magnitude when the signal is doubled"
      (is (< (Math/abs
               (- (* 2.0 (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel))
                  (cqt/calculate-cqt-bin (sine 440.0 1024 2.0) kernel)))
             1.0e-9)))

    (testing "Returns the same magnitude for an inverted signal"
      (is (< (Math/abs
               (- (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel)
                  (cqt/calculate-cqt-bin (sine 440.0 1024 -1.0) kernel)))
             1.0e-9)))

    (testing "Is independent of the phase of the signal"
      (is (< (Math/abs
               (- (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel)
                  (cqt/calculate-cqt-bin (cosine 440.0 1024 1.0) kernel)))
             1.0e-4)))

    (testing "Returns a low magnitude for a sinusoid an octave above"
      (is (< (cqt/calculate-cqt-bin (sine 880.0 1024 1.0) kernel)
             0.01)))

    (testing "Returns a lower magnitude for an adjacent semitone"
      (let [on-frequency (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel)
            semitone-above (cqt/calculate-cqt-bin
                             (sine 466.163762 1024 1.0)
                             kernel)]
        (is (< semitone-above on-frequency))))

    (testing "Returns a near zero magnitude for a constant signal"
      (is (< (cqt/calculate-cqt-bin (vec (repeat 1024 1.0)) kernel)
             1.0e-3)))

    (testing "Returns a near zero magnitude for a signal far from the kernel"
      (is (< (cqt/calculate-cqt-bin (sine 3000.0 1024 1.0) kernel)
             0.01)))

    (testing "Uses only the samples at the center of a longer frame"
      (let [frame (sine 440.0 1024 1.0)
            corrupted (vec (concat (repeat 100 5.0)
                                   (subvec frame 100 924)
                                   (repeat 100 5.0)))]
        (is (= (cqt/calculate-cqt-bin frame kernel)
               (cqt/calculate-cqt-bin corrupted kernel)))))

    (testing "Accepts a frame that is exactly as long as the kernel"
      (let [exact-kernel (cqt/cqt-kernel 440.0 8000.0 182 10.0)
            frame (sine 440.0 182 1.0)]
        (is (< (Math/abs
                 (- 1.0 (cqt/calculate-cqt-bin frame exact-kernel)))
               1.0e-3))))

    (testing "Accepts a frame with an odd number of samples"
      (is (< (Math/abs
               (- 1.0
                  (cqt/calculate-cqt-bin (sine 440.0 1025 1.0) kernel)))
             1.0e-3)))

    (testing "Accepts integer samples"
      (is (= (cqt/calculate-cqt-bin (vec (repeat 1024 1.0)) kernel)
             (cqt/calculate-cqt-bin (vec (repeat 1024 1)) kernel))))

    (testing "Accepts a lazy sequence as the frame"
      (is (= (cqt/calculate-cqt-bin (sine 440.0 1024 1.0) kernel)
             (cqt/calculate-cqt-bin (map identity (sine 440.0 1024 1.0))
                                    kernel))))

    (testing "Measures a sinusoid at the frequency of a different kernel"
      (let [high-kernel (cqt/cqt-kernel 880.0 8000.0 1024 10.0)]
        (is (< (Math/abs
                 (- 1.0
                    (cqt/calculate-cqt-bin
                      (sine 880.0 1024 1.0)
                      high-kernel)))
               1.0e-3))))

    (testing "Throws when the frame is shorter than the kernel"
      (is (thrown?
            IndexOutOfBoundsException
            (cqt/calculate-cqt-bin (vec (repeat 100 0.0)) kernel))))))

(deftest cqt-additional-test
  (let [sine (fn [frequency size amplitude]
               (mapv
                 (fn [n]
                   (* amplitude
                      (Math/sin
                        (/ (* 2.0 Math/PI frequency n)
                           8000.0))))
                 (range size)))
        silent (vec (repeat 1024 0.0))
        peak-bin (fn [magnitudes]
                   (apply max-key
                          #(nth magnitudes %)
                          (range (count magnitudes))))]

    (testing "Returns a vector of frames with the expected keys"
      (let [result (cqt/cqt [silent silent] 8000.0 256 1024 440.0 12 12)]
        (is (vector? result))
        (is (every? #(= #{:frame-index :time-seconds :frequencies
                          :magnitudes}
                        (set (keys %)))
                    result))))

    (testing "Returns a single frame with index zero and time zero"
      (let [result (cqt/cqt [silent] 8000.0 256 1024 440.0 12 12)]
        (is (= 1 (count result)))
        (is (= 0 (:frame-index (first result))))
        (is (= 0.0 (:time-seconds (first result))))))

    (testing "Numbers the frames consecutively"
      (let [result (cqt/cqt (repeat 5 silent) 8000.0 256 1024 440.0 12 12)]
        (is (= [0 1 2 3 4] (mapv :frame-index result)))))

    (testing "Calculates the time of each frame from the hop size"
      (let [result (cqt/cqt (repeat 3 silent) 8000.0 256 1024 440.0 12 12)]
        (is (= [0.0 0.032 0.064] (mapv :time-seconds result)))))

    (testing "Calculates the time of each frame from the sample rate"
      (let [result (cqt/cqt (repeat 4 silent) 16000.0 512 1024 440.0 12 12)]
        (is (= 0.096 (:time-seconds (nth result 3))))))

    (testing "Uses the same frequencies for every frame"
      (let [result (cqt/cqt (repeat 3 silent) 8000.0 256 1024 440.0 12 12)]
        (is (apply = (map :frequencies result)))))

    (testing "Returns the CQT bin frequencies"
      (let [result (cqt/cqt [silent] 8000.0 256 1024 440.0 12 12)]
        (is (= (mapv #(cqt/cqt-frequency % 440.0 12) (range 12))
               (:frequencies (first result))))))

    (testing "Returns one magnitude for every requested bin"
      (let [result (cqt/cqt [silent silent] 8000.0 256 1024 440.0 12 24)]
        (is (every? #(= 24 (count (:magnitudes %))) result))
        (is (every? #(= 24 (count (:frequencies %))) result))))

    (testing "Returns zero magnitudes for every silent frame"
      (let [result (cqt/cqt (repeat 3 silent) 8000.0 256 1024 440.0 12 12)]
        (is (every? #(every? zero? (:magnitudes %)) result))))

    (testing "Returns non-negative magnitudes"
      (let [result (cqt/cqt [(sine 440.0 1024 1.0)]
                            8000.0 256 1024 440.0 12 12)]
        (is (every? #(not (neg? %)) (:magnitudes (first result))))))

    (testing "Finds a 440 Hz sinusoid in the first bin"
      (let [result (cqt/cqt [(sine 440.0 1024 1.0)]
                            8000.0 256 1024 440.0 12 12)
            magnitudes (:magnitudes (first result))]
        (is (= 0 (peak-bin magnitudes)))
        (is (< (Math/abs (- 1.0 (first magnitudes))) 1.0e-3))))

    (testing "Finds a sinusoid in the bin matching its frequency"
      (doseq [bin (range 12)]
        (let [frequency (cqt/cqt-frequency bin 440.0 12)
              result (cqt/cqt [(sine frequency 1024 1.0)]
                              8000.0 256 1024 440.0 12 12)
              magnitudes (:magnitudes (first result))]
          (is (= bin (peak-bin magnitudes)))
          (is (< (Math/abs (- 1.0 (nth magnitudes bin))) 1.0e-2)))))

    (testing "Finds two simultaneous tones in their own bins"
      (let [frame (mapv +
                        (sine 440.0 1024 1.0)
                        (sine (cqt/cqt-frequency 7 440.0 12) 1024 1.0))
            magnitudes (:magnitudes
                         (first
                           (cqt/cqt [frame] 8000.0 256 1024 440.0 12 12)))
            strongest (take 2 (sort-by #(- (nth magnitudes %)) (range 12)))]
        (is (= #{0 7} (set strongest)))))

    (testing "Matches the result of calculating each bin separately"
      (let [frame (sine 523.251131 1024 1.0)
            kernels (cqt/create-cqt-kernels 8000.0 1024 440.0 12 12)
            result (cqt/cqt [frame] 8000.0 256 1024 440.0 12 12)]
        (is (= (mapv #(cqt/calculate-cqt-bin frame %) kernels)
               (:magnitudes (first result))))))

    (testing "Processes every frame independently"
      (let [frame (sine 440.0 1024 1.0)
            alone (cqt/cqt [frame] 8000.0 256 1024 440.0 12 12)
            after-silence (cqt/cqt [silent frame]
                                   8000.0 256 1024 440.0 12 12)]
        (is (= (:magnitudes (first alone))
               (:magnitudes (second after-silence))))))

    (testing "Returns the same magnitudes for a different hop size"
      (let [frames [silent (sine 440.0 1024 1.0)]
            short-hop (cqt/cqt frames 8000.0 128 1024 440.0 12 12)
            long-hop (cqt/cqt frames 8000.0 512 1024 440.0 12 12)]
        (is (= (mapv :magnitudes short-hop)
               (mapv :magnitudes long-hop)))
        (is (not= (mapv :time-seconds short-hop)
                  (mapv :time-seconds long-hop)))))

    (testing "Accepts a lazy sequence of frames"
      (let [frames [silent (sine 440.0 1024 1.0)]]
        (is (= (cqt/cqt frames 8000.0 256 1024 440.0 12 12)
               (cqt/cqt (map identity frames)
                        8000.0 256 1024 440.0 12 12)))))

    (testing "Throws when the frame is too small for the lowest frequency"
      (is (thrown-with-msg?
            clojure.lang.ExceptionInfo
            #"CQT kernel is larger than the frame"
            (cqt/cqt [(vec (repeat 128 0.0))]
                     8000.0 64 128 20.0 12 12))))))