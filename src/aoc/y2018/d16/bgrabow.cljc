(ns aoc.y2018.d16.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   ;[aoc.y2018.d16.data :refer [input answer-1 answer-2]]
   [clojure.test :as t :refer [is testing]]))

(defn addr [registers a b c]
  (assoc registers c (+ (registers a)
                        (registers b))))

(defn addi [registers a b c]
  (assoc registers c (+ (registers a)
                        b)))

(defn mulr [registers a b c]
  (assoc registers c (* (registers a)
                        (registers b))))

(defn muli [registers a b c]
  (assoc registers c (* (registers a)
                        b)))

(defn banr [registers a b c]
  (assoc registers c (bit-and (registers a)
                              (registers b))))

(defn bani [registers a b c]
  (assoc registers c (bit-and (registers a)
                              b)))

(defn borr [registers a b c]
  (assoc registers c (bit-or (registers a)
                             (registers b))))

(defn bori [registers a b c]
  (assoc registers c (bit-or (registers a)
                             b)))

(defn setr [registers a _ c]
  (assoc registers c (registers a)))

(defn seti [registers a _ c]
  (assoc registers c a))

(defn gtir [registers a b c]
  (assoc registers c (if (> a
                            (registers b))
                       1 0)))

(defn gtri [registers a b c]
  (assoc registers c (if (> (registers a)
                            b)
                       1 0)))

(defn gtrr [registers a b c]
  (assoc registers c (if (> (registers a)
                            (registers b))
                       1 0)))

(defn eqir [registers a b c]
  (assoc registers c (if (= a
                            (registers b))
                       1 0)))

(defn eqri [registers a b c]
  (assoc registers c (if (= (registers a)
                            b)
                       1 0)))

(defn eqrr [registers a b c]
  (assoc registers c (if (= (registers a)
                            (registers b))
                       1 0)))

(defn solve-1 [])


(defn solve-2 [])


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

