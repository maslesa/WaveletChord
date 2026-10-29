(ns wavelet-chord.signal)

(defn normalize-signal [samples]
  (let [peak (apply max (map #(Math/abs (double %)) samples))]
    (if (zero? peak)
      (vec samples)
      (mapv #(/ (double %) peak) samples))))

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
