(ns wavelet-chord.evaluation
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [wavelet-chord.signal :as signal]
            [wavelet-chord.cqt :as cqt]
            [wavelet-chord.chroma :as chroma]
            [wavelet-chord.key-estimation :as key-estimation]))

(def dataset-path "resources/dataset/keys.csv")
(def audio-directory "resources/audio")
(def results-path "resources/results/key-evaluation.csv")

(def frame-size 8192)
(def hop-size 2048)

(def fmin 130.8128)
(def bins-per-octave 12)
(def number-of-bins 48)

(def max-frequency
  (* fmin
     (Math/pow 2.0
               (/ (dec number-of-bins)
                  bins-per-octave))))

(defn read-dataset []
  (let [file (io/file dataset-path)]
    (when-not (.exists file)
      (throw (ex-info "Dataset file not found"
                      {:path dataset-path})))
    (let [lines (str/split-lines (slurp file))
          rows (remove str/blank? (rest lines))]
      (mapv
        (fn [line]
          (let [[filename actual-key] (str/split line #"," 2)
                filename (some-> filename str/trim)
                actual-key (some-> actual-key str/trim)]
            (when (or (str/blank? filename)
                      (str/blank? actual-key))
              (throw (ex-info "Invalid dataset row"
                              {:line line})))
            {:filename filename
             :actual-key actual-key}))
        rows))))

(defn validate-dataset [dataset]
  (let [valid-keys
        (set (for [tonic chroma/pitch-classes
                   mode ["major" "minor"]]
               (str tonic " " mode)))
        filenames (map :filename dataset)
        duplicates (->> filenames
                        frequencies
                        (filter (fn [[_ count]] (> count 1)))
                        keys
                        seq)]
    (when (empty? dataset)
      (throw (ex-info "Dataset is empty" {})))

    (when duplicates
      (throw (ex-info "Duplicate audio filenames in dataset"
                      {:filenames duplicates})))

    (doseq [{:keys [filename actual-key]} dataset]
      (when-not (.isFile (io/file audio-directory filename))
        (throw (ex-info "Audio file not found"
                        {:filename filename})))
      (when-not (contains? valid-keys actual-key)
        (throw (ex-info "Invalid key label"
                        {:filename filename
                         :actual-key actual-key}))))
    dataset))

(defn estimate-track-keys [filename]
  (let [audio (signal/load-audio filename)
        samples (signal/normalize-signal (:samples audio))
        sample-rate (:sample-rate audio)
        frames (signal/split-into-frames
                 samples frame-size hop-size)]
    (when (empty? frames)
      (throw (ex-info "Audio is too short for analysis"
                      {:filename filename
                       :sample-count (count samples)})))

    (let [window (signal/hann-window frame-size)
          spectrogram (signal/stft
                        frames sample-rate hop-size window)
          cqt-spectrogram (cqt/cqt
                            frames sample-rate hop-size frame-size
                            fmin bins-per-octave number-of-bins)
          stft-frames (chroma/stft-chroma
                        spectrogram sample-rate frame-size
                        fmin max-frequency)
          cqt-frames (chroma/cqt-chroma cqt-spectrogram)
          stft-profile (chroma/average-chroma stft-frames)
          cqt-profile (chroma/average-chroma cqt-frames)
          stft-result (key-estimation/estimate-key stft-profile)
          cqt-result (key-estimation/estimate-key cqt-profile)]
      {:filename filename
       :duration-seconds (:duration-seconds audio)
       :stft-key (:key stft-result)
       :stft-correlation (:correlation stft-result)
       :cqt-key (:key cqt-result)
       :cqt-correlation (:correlation cqt-result)
       :stft-frames (count stft-frames)
       :cqt-frames (count cqt-frames)})))

(defn key-root [key]
  (first (str/split key #" ")))

(defn key-mode [key]
  (second (str/split key #" ")))

(defn evaluate-result
  [{:keys [actual-key stft-key cqt-key] :as result}]
  (assoc result
    :stft-correct? (= actual-key stft-key)
    :cqt-correct? (= actual-key cqt-key)
    :stft-root-correct? (= (key-root actual-key)
                           (some-> stft-key key-root))
    :cqt-root-correct? (= (key-root actual-key)
                          (some-> cqt-key key-root))
    :stft-mode-correct? (= (key-mode actual-key)
                           (some-> stft-key key-mode))
    :cqt-mode-correct? (= (key-mode actual-key)
                          (some-> cqt-key key-mode))))

(defn calculate-accuracy [results field]
  (if (seq results)
    (/ (double (count (filter field results)))
       (count results))
    0.0))

(defn calculate-metrics [results]
  (let [successful (remove :error results)]
    {:total (count results)
     :successful (count successful)
     :failed (- (count results) (count successful))

     :stft-key-accuracy
     (calculate-accuracy results :stft-correct?)

     :cqt-key-accuracy
     (calculate-accuracy results :cqt-correct?)

     :stft-root-accuracy
     (calculate-accuracy results :stft-root-correct?)

     :cqt-root-accuracy
     (calculate-accuracy results :cqt-root-correct?)

     :stft-mode-accuracy
     (calculate-accuracy results :stft-mode-correct?)

     :cqt-mode-accuracy
     (calculate-accuracy results :cqt-mode-correct?)}))

(defn evaluate-dataset [dataset]
  (mapv
    (fn [{:keys [filename actual-key]}]
      (println "Processing:" filename)
      (try
        (-> (estimate-track-keys filename)
            (assoc :actual-key actual-key)
            evaluate-result)
        (catch Exception exception
          (println "Failed:" (.getMessage exception))
          (evaluate-result
            {:filename filename
             :actual-key actual-key
             :stft-key nil
             :cqt-key nil
             :error (.getMessage exception)}))))
    dataset))

(defn print-metrics [metrics]
  (println "\n--- Evaluation Results ---")
  (println "Total songs:" (:total metrics))
  (println "Successfully processed:" (:successful metrics))
  (println "Failed:" (:failed metrics))

  (println "\nKey accuracy:")
  (println "STFT:"
           (format "%.2f%%"
                   (* 100 (:stft-key-accuracy metrics))))
  (println "CQT:"
           (format "%.2f%%"
                   (* 100 (:cqt-key-accuracy metrics))))

  (println "\nRoot accuracy:")
  (println "STFT:"
           (format "%.2f%%"
                   (* 100 (:stft-root-accuracy metrics))))
  (println "CQT:"
           (format "%.2f%%"
                   (* 100 (:cqt-root-accuracy metrics))))

  (println "\nMode accuracy:")
  (println "STFT:"
           (format "%.2f%%"
                   (* 100 (:stft-mode-accuracy metrics))))
  (println "CQT:"
           (format "%.2f%%"
                   (* 100 (:cqt-mode-accuracy metrics)))))

(defn csv-value [value]
  (let [value (str (or value ""))]
    (str "\""
         (str/replace value "\"" "\"\"")
         "\"")))

(defn write-results [results]
  (let [header ["filename" "actual_key"
                "stft_key" "cqt_key"
                "stft_correct" "cqt_correct"
                "stft_root_correct" "cqt_root_correct"
                "stft_mode_correct" "cqt_mode_correct"
                "stft_correlation" "cqt_correlation"
                "duration_seconds" "error"]
        rows
        (map
          (fn [result]
            (map result
                 [:filename :actual-key
                  :stft-key :cqt-key
                  :stft-correct? :cqt-correct?
                  :stft-root-correct? :cqt-root-correct?
                  :stft-mode-correct? :cqt-mode-correct?
                  :stft-correlation :cqt-correlation
                  :duration-seconds :error]))
          results)
        csv-lines
        (cons header rows)]
    (io/make-parents results-path)
    (spit results-path
          (str
            (str/join
              "\n"
              (map
                (fn [row]
                  (str/join "," (map csv-value row)))
                csv-lines))
            "\n"))
    (println "\nResults saved to:" results-path)))

(defn run-evaluation []
  (println "--- WaveletChord Dataset Evaluation ---")

  (let [dataset (-> (read-dataset)
                    validate-dataset)]
    (println "Dataset size:" (count dataset))
    (println "Audio duration: full track")
    (println "Frame size:" frame-size)
    (println "Hop size:" hop-size)
    (println "Starting evaluation...\n")

    (let [results (evaluate-dataset dataset)
          metrics (calculate-metrics results)]
      (doseq [result results]
        (println
          (format "%-30s | Actual: %-10s | STFT: %-10s | CQT: %-10s"
                  (:filename result)
                  (:actual-key result)
                  (or (:stft-key result) "N/A")
                  (or (:cqt-key result) "N/A"))))

      (print-metrics metrics)
      (write-results results)
      (println "\nEvaluation completed.")
      metrics)))

(defn -main [& _args]
  (run-evaluation))
