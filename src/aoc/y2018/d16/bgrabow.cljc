(ns aoc.y2018.d16.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d16.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def register-addresses #{0 1 2 3})

(defn addr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (+ (registers a)
                          (registers b)))))

(defn addi [registers a b c]
  (when (register-addresses a)
    (assoc registers c (+ (registers a)
                          b))))

(defn mulr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (* (registers a)
                          (registers b)))))

(defn muli [registers a b c]
  (when (register-addresses a)
    (assoc registers c (* (registers a)
                          b))))

(defn banr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (bit-and (registers a)
                                (registers b)))))

(defn bani [registers a b c]
  (when (register-addresses a)
    (assoc registers c (bit-and (registers a)
                                b))))

(defn borr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (bit-or (registers a)
                               (registers b)))))

(defn bori [registers a b c]
  (when (register-addresses a)
    (assoc registers c (bit-or (registers a)
                               b))))

(defn setr [registers a _ c]
  (when (register-addresses a)
    (assoc registers c (registers a))))

(defn seti [registers a _ c]
  (assoc registers c a))

(defn gtir [registers a b c]
  (when (register-addresses b)
    (assoc registers c (if (> a
                              (registers b))
                         1 0))))

(defn gtri [registers a b c]
  (when (register-addresses a)
    (assoc registers c (if (> (registers a)
                              b)
                         1 0))))

(defn gtrr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (if (> (registers a)
                              (registers b))
                         1 0))))

(defn eqir [registers a b c]
  (when (register-addresses b)
    (assoc registers c (if (= a
                              (registers b))
                         1 0))))

(defn eqri [registers a b c]
  (when (register-addresses a)
    (assoc registers c (if (= (registers a)
                              b)
                         1 0))))

(defn eqrr [registers a b c]
  (when (and (register-addresses a)
             (register-addresses b))
    (assoc registers c (if (= (registers a)
                              (registers b))
                         1 0))))

(def all-operations #{addr addi
                      mulr muli
                      banr bani
                      borr bori
                      setr seti
                      gtir gtri gtrr
                      eqir eqri eqrr})

(defn parse-sample [sample-lines]
  (->> sample-lines
       (mapcat #(re-seq #"\d+" %))
       (map u/parse-int)
       (partition 4)
       (map vec)))

(defn parse [input]
  (let [[samples instructions] (->> input
                                    str/split-lines
                                    (partition-all 4)
                                    (split-with #(not (= "" (first %)))))
        parsed-samples (map parse-sample samples)
        parsed-instructions (->> instructions
                                 flatten
                                 (map #(re-seq #"\d+" %))
                                 (filter identity)
                                 (map #(map u/parse-int %))
                                 (map vec))]
    {:samples      parsed-samples
     :instructions parsed-instructions}))

(defn operation-matches? [sample operation]
  (let [[before [_ a b c] after] sample]
    (= (operation before a b c)
       after)))

(defn n-matching-operations [sample]
  (->> all-operations
       (filter (partial operation-matches? sample))
       count))

(defn solve-1 []
  (->> input
       parse
       :samples
       (map n-matching-operations)
       (filter #(>= % 3))
       count))

(defn add-matching-operations [opcode-map sample]
  (let [[_ [opcode & _] _] sample]
    (update opcode-map
            opcode
            #(apply conj %
                    (filter (partial operation-matches? sample) all-operations)))))

(defn filter-definite-mappings [uncertain-mappings]
  (keep (fn [[k v]]
          (when (= 1 (count v))
            [k (first v)]))
        uncertain-mappings))

(defn remove-definite-ops [uncertain-mappings new-definite-mappings]
  (into {} (map (fn [[k v]]
                  [k (apply disj v (vals new-definite-mappings))])
                uncertain-mappings)))

(defn deduce-opcode-mapping [filtered-opcode-map]
  (loop [definite-mappings {}
         uncertain-mappings filtered-opcode-map]
    (if (some #(= 1 (count (second %))) uncertain-mappings)
      (let [new-definite-mappings (into definite-mappings
                                        (filter-definite-mappings uncertain-mappings))]
        (recur new-definite-mappings
               (remove-definite-ops uncertain-mappings new-definite-mappings)))
      definite-mappings)))

(defn solve-2 []
  (let [{:keys [samples instructions]} (parse input)
        possible-opcodes (zipmap (range 16) (repeat #{}))
        filtered-opcode-map (reduce add-matching-operations
                                    possible-opcodes
                                    samples)
        final-opcode-map (deduce-opcode-mapping filtered-opcode-map)]
    (-> (reduce (fn [r [opcode a b c]]
                  ((final-opcode-map opcode) r a b c))
                [0 0 0 0]
                instructions)
        (get 0))))

(deftest part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

