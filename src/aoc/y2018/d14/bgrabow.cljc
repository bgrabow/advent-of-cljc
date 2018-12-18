(ns aoc.y2018.d14.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d14.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))





(def initial-state {:recipes [3 7]
                    :elves   [0 1]})

(defn digits [n]
  (p ::digits
     (if (< n 10)
       [n]
       (conj (digits (quot n 10)) (rem n 10)))))

(defn digits-slow [n]
  (-> (str n)
      (str/split #"")
      (#(map u/parse-int %))))

;(digits-slow 123)

(defn adjust-idx [len idx]
  (->>
    (iterate #(- % len) idx)
    (filter #(< % len))
    first))

(defn step-fast? [{:keys [elves recipes]}]
  (p ::step-fast?
     (let [elf1 (elves 0)
           elf2 (elves 1)
           recipe1 (recipes elf1)
           recipe2 (recipes elf2)
           ;score (->> elves
           ;           (map recipes)
           ;           (apply +))
           score (+ recipe1 recipe2)
           recipes' (apply conj recipes (digits score))
           recipes'-size (count recipes')
           elf1' (adjust-idx recipes'-size (+ elf1 1 recipe1))
           elf2' (adjust-idx recipes'-size (+ elf2 1 recipe2))]
       ;elves' (vector (adjust-idx (count recipes') (+ elf1 1 recipe1))
       ;               (adjust-idx (count recipes') (+ elf2 1 recipe2)))]
       {:recipes recipes'
        :elves   (vector elf1' elf2')})))

(defn step [{:keys [elves recipes]}]
  (p ::step-fn (let [score (p ::calc-score (+ (recipes (elves 0)) (recipes (elves 1))))
                     new-recipes (p ::add-recipes (apply conj recipes (digits score)))
                     new-elves (p ::move-elves (map (fn [elf] (rem (+ elf 1 (recipes elf))
                                                                   (count new-recipes)))
                                                    elves))]
                 (p ::combine-new-state {:recipes new-recipes
                                         :elves   new-elves}))))

(def my-input "030121")

(defn solve-1 []
  (let [input-num (u/parse-int my-input)]
    (->> (iterate step initial-state)
         (filter #(>= (count (:recipes %)) (+ 10 input-num)))
         first
         :recipes
         (#(subvec % input-num (+ 10 input-num)))
         (map str)
         str/join)))

(defn has-input-at-end-or-decend [input-size input {:keys [recipes]}]
  (p ::has-input-at-end-or-decend
     (let [recipes-size (count recipes)
           start (max 0 (- recipes-size input-size))
           end recipes-size
           decstart (max 0 (dec start))
           decend (max 0 (dec end))]
       (p ::has-match? (cond
                         (= input (subvec recipes start end))
                         start
                         (= input (subvec recipes decstart decend))
                         decstart)))))

(defn solve-2 []
  ;(time                                                     ; TODO remove this
  (let [input-vec (mapv (comp u/parse-int str) "030121")
        input-size (count input-vec)]
    (println "Starting search for" input-vec)
    (->> (iterate step initial-state)
         (some #(has-input-at-end-or-decend input-size input-vec %)))))

(tufte/add-basic-println-handler! {})
(profile {}
         (p ::solve-2 (solve-2)))

;(time                                                       ; TODO remove this
;  (let [input-vec (mapv (comp u/parse-int str) my-input #_(first (nth (seq test2-inputs) 3)))]
;    (println "Starting search for" input-vec)
;    (->> (iterate step-fast? initial-state)
;         (some #(has-input-at-end-or-decend input-vec %)))))

;(loop [elves (:elves initial-state)
;       recipes (:recipes initial-state)
;       target (mapv (comp u/parse-int str) (first (nth (seq test2-inputs) 0)))
;       search-start (- (count (:recipes state)) (count target))
;       search-end (count (:recipes state))]
;  (let [score
;        (->> elves
;             (map recipes)
;             (apply +))
;        new-recipes (apply conj recipes (digits score))
;        new-elves (map (fn [elf] (rem (+ elf 1 (recipes elf))
;                                      (count new-recipes)))
;                       elves)]))

(comment                                                    ; TODO remove this
  (def my-input "030121")
  (def test-inputs {"9"    "5158916779"
                    "5"    "0124515891"
                    "18"   "9251071085"
                    "2018" "5941429882"})
  (def test2-inputs {"51589" "9"
                     "01245" "5"
                     "92510" "18"
                     "59414" "2018"}))

;(deftest part-1
;  (is (= (str answer-1)
;         (str (solve-1)))))
;
;(deftest part-2
;  (is (= (str answer-2)
;         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

