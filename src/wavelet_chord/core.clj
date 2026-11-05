(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]
            [wavelet-chord.cqt :as cqt]))

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
        cqt-result (cqt/cqt frames sample-rate hop-size frame-size fmin bins-per-octave number-of-bins)
        first-stft-spectrum (:magnitudes (first spectrogram))
        first-cqt-spectrum (:magnitudes (first cqt-result))
        cqt-frequencies (:frequencies (first cqt-result))]

    (println "\n--- Audio Information ---")
    (println "Sample rate:" sample-rate "Hz")
    (println "Channels:" (:channels audio))
    (println "Duration:" (format "%.2f" (:duration-seconds audio)) "seconds")
    (println "Total samples:" (count samples))

    (println "\n--- Comparison Configuration ---")
    (println "Segment duration:" comparison-duration "seconds")
    (println "Frame size:" frame-size)
    (println "Hop size:" hop-size)
    (println "Number of frames:" (count frames))

    (println "\n--- STFT ---")
    (println "Number of spectra:" (count spectrogram))
    (println "Frequency bins per spectrum:" (count first-stft-spectrum))
    (println "Frequency resolution:" (signal/frequency-for-bin 1 sample-rate frame-size) "Hz")
    (println "First 12 magnitudes:" (take 12 first-stft-spectrum))

    (println "\n--- CQT ---")
    (println "Number of spectra:" (count cqt-result))
    (println "Frequency bins per spectrum:" (count first-cqt-spectrum))
    (println "Lowest frequency:" (first cqt-frequencies) "Hz")
    (println "Highest frequency:" (last cqt-frequencies) "Hz")
    (println "Bins per octave:" bins-per-octave)
    (println "First 12 frequencies:" (take 12 cqt-frequencies))
    (println "First 12 magnitudes:" (take 12 first-cqt-spectrum))

    (println "\n--- STFT vs CQT ---")
    (println "STFT bins:" (count first-stft-spectrum))
    (println "CQT bins:" (count first-cqt-spectrum))
    (println "STFT frames:" (count spectrogram))
    (println "CQT frames:" (count cqt-result))
    (println "STFT and CQT use the same frames:" (= (count spectrogram) (count cqt-result)))

    (println "\nComparison completed.")))
