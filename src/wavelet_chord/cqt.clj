(ns wavelet-chord.cqt)

(defn cqt-frequency
  "Calculates the frequency of a CQT bin."
  [bin fmin bins-per-octave]
  (* fmin
     (Math/pow 2.0
               (/ (double bin) bins-per-octave))))

(defn cqt-kernel
  "Creates a windowed complex sinusoidal kernel for one frequency."
  [frequency sample-rate frame-size q-factor]
  (let [kernel-size (long
                      (Math/ceil
                        (/ (* q-factor sample-rate) frequency)))]
    (when (> kernel-size frame-size)
      (throw
        (ex-info "CQT kernel is larger than the frame"
                 {:frequency frequency
                  :kernel-size kernel-size
                  :frame-size frame-size})))

    (let [real-kernel (double-array kernel-size)
          imaginary-kernel (double-array kernel-size)
          window-sum
          (loop [n 0
                 total 0.0]
            (if (< n kernel-size)
              (let [window
                    (* 0.5
                       (- 1.0
                          (Math/cos
                            (/ (* 2.0 Math/PI n)
                               (dec kernel-size)))))
                    angle
                    (/ (* 2.0 Math/PI frequency n)
                       sample-rate)]

                (aset-double real-kernel n
                             (* window (Math/cos angle)))

                (aset-double imaginary-kernel n
                             (* window (Math/sin angle)))

                (recur (inc n) (+ total window)))
              total))]

      {:frequency frequency
       :kernel-size kernel-size
       :real real-kernel
       :imaginary imaginary-kernel
       :normalization (/ 2.0 window-sum)})))

(defn create-cqt-kernels
  "Creates a bank of CQT kernels covering the requested frequency range."
  [sample-rate frame-size fmin bins-per-octave number-of-bins]
  (let [q-factor
        (/ 1.0
           (- (Math/pow 2.0
                        (/ 1.0 bins-per-octave))
              1.0))]

    (mapv
      (fn [bin]
        (let [frequency
              (cqt-frequency bin fmin bins-per-octave)]
          (cqt-kernel frequency
                      sample-rate
                      frame-size
                      q-factor)))
      (range number-of-bins))))

(defn calculate-cqt-bin
  "Calculates the magnitude for one CQT bin and one audio frame."
  [frame kernel]
  (let [frame-size (count frame)
        kernel-size (:kernel-size kernel)
        start-index (quot (- frame-size kernel-size) 2)
        real-kernel (:real kernel)
        imaginary-kernel (:imaginary kernel)]

    (loop [i 0
           real-sum 0.0
           imaginary-sum 0.0]

      (if (< i kernel-size)
        (let [sample
              (double
                (nth frame (+ start-index i)))

              real-value
              (* sample (aget ^doubles real-kernel i))

              imaginary-value
              (* sample (aget ^doubles imaginary-kernel i))]

          (recur (inc i)
                 (+ real-sum real-value)
                 (+ imaginary-sum imaginary-value)))

        (* (:normalization kernel)
           (Math/sqrt
             (+ (* real-sum real-sum)
                (* imaginary-sum imaginary-sum))))))))

(defn cqt
  "Computes the Constant-Q Transform for a collection of audio frames."
  [frames sample-rate hop-size
   frame-size fmin bins-per-octave number-of-bins]

  (let [kernels
        (create-cqt-kernels
          sample-rate
          frame-size
          fmin
          bins-per-octave
          number-of-bins)

        frequencies
        (mapv :frequency kernels)]

    (mapv
      (fn [index frame]
        {:frame-index index
         :time-seconds
         (/ (* index hop-size) sample-rate)
         :frequencies frequencies
         :magnitudes
         (mapv
           (fn [kernel]
             (calculate-cqt-bin frame kernel))
           kernels)})
      (range)
      frames)))
