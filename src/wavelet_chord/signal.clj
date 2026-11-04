(ns wavelet-chord.signal
  (:require [clojure.java.io :as io])
  (:import [javax.sound.sampled AudioSystem AudioFormat$Encoding AudioFormat]
           [java.io ByteArrayOutputStream]
           [org.jtransforms.fft DoubleFFT_1D]))

(defn load-audio [filename]
  (let [file (io/file "resources" "audio" filename)]
    (when-not (.isFile file)
      (throw (ex-info "Audio file not found"
                      {:path (.getPath file)})))
    (with-open [input (AudioSystem/getAudioInputStream file)]
      (let [format (.getFormat input)
            sample-rate (.getSampleRate format)
            channels (.getChannels format)
            target-format (AudioFormat.
                            AudioFormat$Encoding/PCM_SIGNED
                            sample-rate 16 channels
                            (* channels 2) sample-rate false)]
        (when-not (AudioSystem/isConversionSupported
                    target-format format)
          (throw (ex-info "Unsupported audio format"
                          {:format (str format)})))
        (with-open [pcm (AudioSystem/getAudioInputStream
                          target-format input)
                    output (ByteArrayOutputStream.)]
          (let [buffer (byte-array 4096)]
            (loop []
              (let [n (.read pcm buffer)]
                (when (pos? n)
                  (.write output buffer 0 n)
                  (recur))))
            (let [bytes (.toByteArray output)
                  frames (quot (alength bytes) (* channels 2))
                  samples
                  (mapv
                    (fn [frame]
                      (let [values
                            (for [channel (range channels)
                                  :let [i (* 2 (+ (* frame channels) channel))
                                        low (bit-and 0xFF (aget bytes i))
                                        high (aget bytes (inc i))
                                        value (short
                                                (bit-or low
                                                        (bit-shift-left high 8)))]]
                              (/ (double value) 32768.0))]
                        (/ (reduce + values) channels)))
                    (range frames))]
              {:samples samples
               :sample-rate sample-rate
               :channels channels
               :duration-seconds (/ (double frames) sample-rate)})))))))

(defn normalize-signal [samples]
  (let [peak (apply max (map #(Math/abs (double %)) samples))]
    (if (zero? peak)
      (vec samples)
      (mapv #(/ (double %) (double peak)) samples))))

(defn mean-amplitude [samples]
  (if (empty? samples)
    0.0
    (/ (reduce + (map #(Math/abs (double %)) samples))
       (count samples))))

(defn rms-amplitude [samples]
  (if (empty? samples)
    0.0
    (Math/sqrt
      (/ (reduce + (map #(let [x (double %)]
                           (* x x))
                        samples))
         (count samples)))))

(defn split-into-frames [samples frame-size hop-size]
  (when (or (not (pos? frame-size))
            (not (pos? hop-size)))
    (throw (ex-info "Frame size and hop size must be positive"
                    {:frame-size frame-size
                     :hop-size hop-size})))

  (let [samples (vec samples)
        sample-count (count samples)
        frame-starts (range 0
                            (inc (- sample-count frame-size))
                            hop-size)]
    (mapv (fn [start]
            (subvec samples start (+ start frame-size)))
          frame-starts)))

(defn hann-window [frame-size]
  (when-not (> frame-size 1)
    (throw (ex-info "Frame size must be greater than 1"
                    {:frame-size frame-size})))

  (mapv (fn [n]
          (* 0.5
             (- 1.0
                (Math/cos
                  (/ (* 2.0 Math/PI n)
                     (dec frame-size))))))
        (range frame-size)))

(defn apply-window [frame window]
  (when-not (= (count frame) (count window))
    (throw (ex-info "Frame and window must have the same size"
                    {:frame-size (count frame)
                     :window-size (count window)})))

  (mapv * frame window))

(defn fft-magnitudes [frame]
  (let [frame-size (count frame)
        fft (DoubleFFT_1D. (long frame-size))
        data (double-array (* 2 frame-size))]

    (when (zero? frame-size)
      (throw (ex-info "Frame cannot be empty" {})))

    (doseq [i (range frame-size)]
      (aset-double data (* 2 i) (double (nth frame i))))

    (.complexForward fft data)

    (mapv (fn [i]
            (let [real (aget data (* 2 i))
                  imaginary (aget data (inc (* 2 i)))]
              (Math/sqrt (+ (* real real)
                            (* imaginary imaginary)))))
          (range (inc (quot frame-size 2))))))