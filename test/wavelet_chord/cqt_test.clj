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