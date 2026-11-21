(ns wavelet-chord.wavelet)

(def omega0 6.0)
(def support-width 3.0)

(defn wavelet-frequency [bin fmin bins-per-octave]
  (* fmin
     (Math/pow 2.0
               (/ bin (double bins-per-octave)))))

(defn create-morlet-kernel [frequency sample-rate]
  (when (or (not (pos? frequency))
            (not (pos? sample-rate))
            (>= frequency (/ sample-rate 2.0)))
    (throw (ex-info "Invalid wavelet frequency"
                    {:frequency frequency
                     :sample-rate sample-rate})))

  (let [sigma (/ omega0 (* 2.0 Math/PI frequency))
        half-size (int (Math/ceil
                         (* support-width sigma sample-rate)))
        kernel-size (inc (* 2 half-size))
        correction (Math/exp (- (* 0.5 omega0 omega0)))
        real (double-array kernel-size)
        imaginary (double-array kernel-size)
        normalization
        (loop [index 0
               total 0.0]
          (if (< index kernel-size)
            (let [time (/ (- index half-size)
                          (double sample-rate))
                  scaled-time (/ time sigma)
                  gaussian (Math/exp
                             (* -0.5 scaled-time scaled-time))
                  angle (* 2.0 Math/PI frequency time)
                  real-value (* gaussian
                                (- (Math/cos angle) correction))
                  imaginary-value (* gaussian (Math/sin angle))]
              (aset-double real index real-value)
              (aset-double imaginary index imaginary-value)
              (recur (inc index) (+ total gaussian)))
            total))]

    (when (> kernel-size 0)
      (dotimes [index kernel-size]
        (aset-double real index
                     (/ (aget real index) normalization))
        (aset-double imaginary index
                     (/ (aget imaginary index) normalization))))

    {:frequency frequency
     :kernel-size kernel-size
     :real real
     :imaginary imaginary}))

(defn create-wavelet-kernels
  [sample-rate frame-size fmin bins-per-octave number-of-bins]
  (when (or (not (pos? frame-size))
            (not (pos? bins-per-octave))
            (not (pos? number-of-bins)))
    (throw (ex-info "Invalid wavelet configuration"
                    {:frame-size frame-size
                     :bins-per-octave bins-per-octave
                     :number-of-bins number-of-bins})))

  (mapv
    (fn [bin]
      (let [frequency (wavelet-frequency
                        bin fmin bins-per-octave)
            kernel (create-morlet-kernel frequency sample-rate)]
        (when (> (:kernel-size kernel) frame-size)
          (throw (ex-info "Wavelet kernel is larger than the frame"
                          {:frequency frequency
                           :kernel-size (:kernel-size kernel)
                           :frame-size frame-size})))
        kernel))
    (range number-of-bins)))

(defn calculate-wavelet-bin [frame kernel]
  (let [frame-size (count frame)
        kernel-size (:kernel-size kernel)
        start-index (quot (- frame-size kernel-size) 2)
        real (:real kernel)
        imaginary (:imaginary kernel)
        [real-sum imaginary-sum]
        (loop [index 0
               real-sum 0.0
               imaginary-sum 0.0]
          (if (< index kernel-size)
            (let [sample (double (nth frame (+ start-index index)))]
              (recur
                (inc index)
                (+ real-sum (* sample (aget real index)))
                (+ imaginary-sum
                   (* sample (aget imaginary index)))))
            [real-sum imaginary-sum]))]
    (Math/sqrt
      (+ (* real-sum real-sum)
         (* imaginary-sum imaginary-sum)))))

(defn wavelet-transform
  [frames sample-rate hop-size frame-size
   fmin bins-per-octave number-of-bins]
  (let [kernels (create-wavelet-kernels
                  sample-rate frame-size
                  fmin bins-per-octave number-of-bins)]
    (mapv
      (fn [frame-index frame]
        {:frame-index frame-index
         :time-seconds (/ (* frame-index hop-size)
                          (double sample-rate))
         :frequencies (mapv :frequency kernels)
         :magnitudes (mapv
                       #(calculate-wavelet-bin frame %)
                       kernels)})
      (range)
      frames)))
