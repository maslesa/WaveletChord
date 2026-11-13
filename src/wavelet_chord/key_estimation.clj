
(ns wavelet-chord.key-estimation
  (:require [wavelet-chord.chroma :as chroma]))

(def major-profile
  "Krumhansl-Kessler reference profile for major keys."
  [6.35 2.23 3.48 2.33 4.38 4.09
   2.52 5.19 2.39 3.66 2.29 2.88])

(def minor-profile
  "Krumhansl-Kessler reference profile for minor keys."
  [6.33 2.68 3.52 5.38 2.60 3.53
   2.54 4.75 3.98 2.69 3.34 3.17])

(defn pearson-correlation
  "Calculates Pearson correlation between two numeric vectors."
  [first-vector second-vector]
  (when-not (= (count first-vector) (count second-vector))
    (throw
      (ex-info "Vectors must have the same length"
               {:first-size (count first-vector)
                :second-size (count second-vector)})))

  (let [first-mean
        (/ (reduce + first-vector)
           (double (count first-vector)))

        second-mean
        (/ (reduce + second-vector)
           (double (count second-vector)))

        first-centered
        (mapv #(- (double %) first-mean)
              first-vector)

        second-centered
        (mapv #(- (double %) second-mean)
              second-vector)

        numerator
        (reduce +
                (map *
                     first-centered
                     second-centered))

        first-squared
        (reduce +
                (map #(* % %)
                     first-centered))

        second-squared
        (reduce +
                (map #(* % %)
                     second-centered))

        denominator
        (Math/sqrt
          (* first-squared second-squared))]

    (if (zero? denominator)
      0.0
      (/ numerator denominator))))

(defn rotate-profile
  "Rotates a reference profile to match the selected tonic."
  [profile tonic-index]
  (mapv
    (fn [pitch-index]
      (nth profile
           (mod (- pitch-index tonic-index) 12)))
    (range 12)))

(defn create-key-candidates
  "Creates all 24 major and minor key candidates."
  [chroma-profile]

  (let [pitch-classes chroma/pitch-classes]

    (for [mode [:major :minor]
          tonic-index (range 12)
          :let [reference-profile
                (if (= mode :major)
                  major-profile
                  minor-profile)

                rotated-profile
                (rotate-profile reference-profile
                                tonic-index)

                correlation
                (pearson-correlation
                  chroma-profile
                  rotated-profile)]]

      {:tonic (nth pitch-classes tonic-index)
       :mode mode
       :key (str (nth pitch-classes tonic-index)
                 " "
                 (name mode))
       :correlation correlation})))

(defn estimate-key
  "Estimates the musical key from an average chroma profile.

   Returns the best matching key and all 24 candidates.
   Returns nil when the chroma profile contains no variation."
  [chroma-profile]

  (when-not (= 12 (count chroma-profile))
    (throw
      (ex-info "Chroma profile must contain exactly 12 values"
               {:profile-size (count chroma-profile)})))

  (when-not (every? number? chroma-profile)
    (throw
      (ex-info "Chroma profile must contain numeric values"
               {:profile chroma-profile})))

  (let [values (mapv double chroma-profile)
        candidates
        (->> (create-key-candidates values)
             (sort-by :correlation >)
             vec)]

    (when (pos?
            (reduce +
                    (map #(Math/abs %)
                         (map #(- % (first values))
                              values))))

      (let [best-match (first candidates)]

        {:key (:key best-match)
         :tonic (:tonic best-match)
         :mode (:mode best-match)
         :correlation (:correlation best-match)
         :candidates candidates}))))

(defn estimate-key-from-frames
  "Estimates the musical key directly from chroma frames."
  [chroma-frames]

  (estimate-key
    (chroma/average-chroma chroma-frames)))

