
(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]
            [wavelet-chord.cqt :as cqt]
            [wavelet-chord.chroma :as chroma]
            [wavelet-chord.key-estimation :as key-estimation]))

(defn print-chroma-profile [title profile]
  (println title)
  (doseq [{:keys [pitch-class value]}
          (chroma/rank-pitch-classes profile)]
    (println (format "%-3s %.4f" pitch-class value))))

(defn print-key-estimation [title result]
  (println title)
  (if result
    (do
      (println "Estimated key:" (:key result))
      (println "Correlation:" (format "%.4f" (:correlation result)))
      (println "Top 5 candidates:")
      (doseq [candidate (take 5 (:candidates result))]
        (println (format "%-8s %.4f"
                         (:key candidate)
                         (:correlation candidate)))))
    (println "Could not estimate key.")))

(defn -main [& _args]
  (let [audio (signal/load-audio "Tri_Poljupca_Za_Srecu.wav")
        samples (:samples audio)
        normalized (signal/normalize-signal samples)
        sample-rate (:sample-rate audio)
        comparison-duration 30
        sample-count (int (* comparison-duration sample-rate))
        segment-samples (take sample-count normalized)
        frame-size 8192
        hop-size 2048
        frames (signal/split-into-frames segment-samples frame-size hop-size)
        window (signal/hann-window frame-size)
        spectrogram (signal/stft frames sample-rate hop-size window)
        fmin 130.8128
        bins-per-octave 12
        number-of-bins 48
        max-frequency (* fmin
                         (Math/pow 2.0
                                   (/ (dec number-of-bins)
                                      bins-per-octave)))
        cqt-result (cqt/cqt frames sample-rate hop-size frame-size
                            fmin bins-per-octave number-of-bins)
        stft-chroma (chroma/stft-chroma spectrogram sample-rate
                                        frame-size fmin max-frequency)
        cqt-chroma (chroma/cqt-chroma cqt-result)
        average-stft-chroma (chroma/average-chroma stft-chroma)
        average-cqt-chroma (chroma/average-chroma cqt-chroma)
        stft-key (key-estimation/estimate-key average-stft-chroma)
        cqt-key (key-estimation/estimate-key average-cqt-chroma)]

    (println "\n--- Audio Information ---")
    (println "Sample rate:" sample-rate "Hz")
    (println "Channels:" (:channels audio))
    (println "Duration:"
             (format "%.2f" (:duration-seconds audio))
             "seconds")
    (println "Total samples:" (count samples))

    (println "\n--- Configuration ---")
    (println "Segment duration:" comparison-duration "seconds")
    (println "Frame size:" frame-size)
    (println "Hop size:" hop-size)
    (println "Number of frames:" (count frames))
    (println "CQT minimum frequency:" (format "%.2f" fmin) "Hz")
    (println "CQT maximum frequency:" (format "%.2f" max-frequency) "Hz")
    (println "CQT bins:" number-of-bins)

    (println "\n--- Chroma Extraction ---")
    (println "STFT chroma frames:" (count stft-chroma))
    (println "CQT chroma frames:" (count cqt-chroma))
    (println "Pitch classes:" chroma/pitch-classes)

    (println "\n--- First STFT Chroma Frame ---")
    (when-let [first-frame (first stft-chroma)]
      (println (:values first-frame)))

    (println "\n--- First CQT Chroma Frame ---")
    (when-let [first-frame (first cqt-chroma)]
      (println (:values first-frame)))

    (println "\n--- Average STFT Chroma Profile ---")
    (print-chroma-profile "STFT pitch class strengths:"
                          average-stft-chroma)

    (println "\n--- Average CQT Chroma Profile ---")
    (print-chroma-profile "CQT pitch class strengths:"
                          average-cqt-chroma)

    (println "\n--- STFT vs CQT Chroma ---")
    (println "STFT chroma dimensions:"
             (count stft-chroma) "x"
             (count chroma/pitch-classes))
    (println "CQT chroma dimensions:"
             (count cqt-chroma) "x"
             (count chroma/pitch-classes))
    (println "Same number of frames:"
             (= (count stft-chroma) (count cqt-chroma)))

    (println "\n--- Musical Key Estimation ---")
    (print-key-estimation "STFT key estimation:" stft-key)
    (print-key-estimation "CQT key estimation:" cqt-key)

    (println "\nKey estimation completed.")
    (shutdown-agents)))