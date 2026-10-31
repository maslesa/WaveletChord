(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]))

(defn -main [& _args]
  (let [audio (signal/load-audio "Tri_Poljupca_Za_Srecu.wav")
        samples (:samples audio)
        normalized (signal/normalize-signal samples)]

    (println "\n--- Audio Information ---")
    (println "Sample rate:" (:sample-rate audio) "Hz")
    (println "Channels:" (:channels audio))
    (println "Duration:" (format "%.2f" (:duration-seconds audio)) "seconds")
    (println "Number of samples:" (count samples))

    (println "\n--- Signal Analysis ---")
    (println "Mean amplitude:" (signal/mean-amplitude samples))
    (println "RMS amplitude:" (signal/rms-amplitude samples))

    (println "\n--- Normalized Signal ---")
    (println "First 10 samples:" (take 10 normalized))
    (println "Normalized RMS:" (signal/rms-amplitude normalized))))