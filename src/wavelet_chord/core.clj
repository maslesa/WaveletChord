(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]))

(defn -main [& _args]
  (let [audio (signal/load-audio "Tri_Poljupca_Za_Srecu.wav")
        samples (:samples audio)
        normalized (signal/normalize-signal samples)
        frame-size 2048
        hop-size 512
        frames (signal/split-into-frames normalized frame-size hop-size)
        window (signal/hann-window frame-size)
        spectrogram (signal/stft frames
                                 (:sample-rate audio)
                                 hop-size
                                 window)
        first-spectrum (:magnitudes (first spectrogram))]

    (println "\n--- Audio Information ---")
    (println "Sample rate:" (:sample-rate audio) "Hz")
    (println "Channels:" (:channels audio))
    (println "Duration:" (format "%.2f" (:duration-seconds audio)) "seconds")
    (println "Number of samples:" (count samples))

    (println "\n--- Signal Analysis ---")
    (println "Mean amplitude:" (signal/mean-amplitude samples))
    (println "RMS amplitude:" (signal/rms-amplitude samples))

    (println "\n--- Framing ---")
    (println "Frame size:" frame-size)
    (println "Hop size:" hop-size)
    (println "Number of frames:" (count frames))

    (println "\n--- Hann Window ---")
    (println "Window size:" (count window))
    (println "First coefficient:" (first window))
    (println "Middle coefficient:" (nth window (quot frame-size 2)))
    (println "Last coefficient:" (last window))

    (println "\n--- STFT ---")
    (println "Number of spectra:" (count spectrogram))
    (println "Frequency bins per spectrum:" (count first-spectrum))
    (println "Frequency resolution:"
             (signal/frequency-for-bin 1
                                       (:sample-rate audio)
                                       frame-size)
             "Hz")

    (println "\n--- First Spectrum ---")
    (println "Frame index:" (:frame-index (first spectrogram)))
    (println "Time:" (:time-seconds (first spectrogram)) "seconds")
    (println "First 10 magnitudes:"
             (take 10 first-spectrum))))