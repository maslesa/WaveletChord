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
        first-frame (first frames)
        windowed-frame (signal/apply-window first-frame window)]

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
    (println "First 10 coefficients:" (take 10 window))
    (println "First coefficient:" (first window))
    (println "Middle coefficient:" (nth window (quot frame-size 2)))
    (println "Last coefficient:" (last window))

    (println "\n--- Windowed Frame ---")
    (println "First 10 samples:" (take 10 windowed-frame))
    (println "Original frame RMS:" (signal/rms-amplitude first-frame))
    (println "Windowed frame RMS:" (signal/rms-amplitude windowed-frame))))