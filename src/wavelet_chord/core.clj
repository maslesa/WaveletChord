(ns wavelet-chord.core
  (:require [wavelet-chord.signal :as signal]))

(defn -main [& _args]
  (let [audio (signal/load-audio "Tri_Poljupca_Za_Srecu.wav")]
    (println "Sample rate:" (:sample-rate audio))
    (println "Channels:" (:channels audio))
    (println "Duration:" (:duration-seconds audio) "seconds")
    (println "First 10 samples:"
             (take 10 (:samples audio)))))