(ns aoc.y2018.d14.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d14.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def my-input "030121")
(def test-inputs {"9"    "5158916779"
                  "5"    "0124515891"
                  "18"   "9251071085"
                  "2018" "5941429882"})
(def test2-inputs {"51589" "9"
                   "01245" "5"
                   "92510" "18"
                   "59414" "2018"})


(def initial-state {:recipes [3 7]
                    :elves   '(0 1)})

(defn digits [n]
  (if (< n 10)
    [n]
    (conj (digits (quot n 10)) (rem n 10))))

(defn step [{:keys [elves recipes]}]
  (let [score
        (->> elves
             (map recipes)
             (apply +))
        new-recipes (apply conj recipes (digits score))
        new-elves (map (fn [elf] (rem (+ elf 1 (recipes elf))
                                      (count new-recipes)))
                       elves)]
    {:recipes new-recipes
     :elves   new-elves}))

(defn solve-1 []
  (let [input-num (u/parse-int my-input)]
    (->> (iterate step initial-state)
         (filter #(>= (count (:recipes %)) (+ 10 input-num)))
         first
         :recipes
         (#(subvec % input-num (+ 10 input-num)))
         (map str)
         str/join)))

(defn solve-2 [])
;; TODO


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

