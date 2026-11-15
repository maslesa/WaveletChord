(ns wavelet-chord.evaluation
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def dataset-path "resources/dataset/keys.csv")

(defn read-dataset []
  (let [lines (-> dataset-path
                  slurp
                  str/split-lines)
        rows (rest lines)]
    (mapv
      (fn [line]
        (let [[filename key] (str/split line #"," 2)
              filename (some-> filename str/trim)
              key (some-> key str/trim)]
          (when (or (str/blank? filename)
                    (str/blank? key))
            (throw (ex-info "Invalid dataset row"
                            {:line line})))
          {:filename filename
           :actual-key key}))
      (remove str/blank? rows))))

(defn validate-dataset [dataset]
  (let [valid-keys
        (set (for [tonic ["C" "C#" "D" "D#" "E" "F"
                          "F#" "G" "G#" "A" "A#" "B"]
                   mode ["major" "minor"]]
               (str tonic " " mode)))]
    (doseq [{:keys [filename actual-key]} dataset]
      (when-not (.exists (io/file "resources/audio" filename))
        (throw (ex-info "Audio file not found"
                        {:filename filename})))
      (when-not (contains? valid-keys actual-key)
        (throw (ex-info "Invalid key label"
                        {:filename filename
                         :actual-key actual-key}))))
    dataset))
