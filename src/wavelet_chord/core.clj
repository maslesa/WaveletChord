(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]))

(defn -main [& _args]
  (let [samples [0.2 -0.4 0.6 -0.1 0.3]
        normalized (signal/normalize-signal samples)]
    (println "Original signal:" samples)
    (println "Normalized signal:" normalized)
    (println "Mean amplitude:" (signal/mean-amplitude samples))
    (println "RMS amplitude:" (signal/rms-amplitude samples))))