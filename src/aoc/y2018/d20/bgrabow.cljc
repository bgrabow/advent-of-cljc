(ns aoc.y2018.d20.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d20.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def step {\N [0 -1]
           \S [0 1]
           \E [1 0]
           \W [-1 0]})

(defn letter [c acc]
  (let [{:keys [loc factory]} acc]
    (let [next-room (mapv + loc (step c))]
      (if (factory next-room)  ; Assume no cycles.
        (assoc acc :loc next-room)
        (-> acc
            (update-in [:factory loc] #(into #{next-room} %))
            (assoc :loc next-room))))))

(defn open-paren [acc]
  (update acc :intersections conj (:loc acc)))

(defn close-paren [acc]
  (-> acc
      (assoc :loc (-> acc :intersections peek))
      (update :intersections pop)))

(defn pipe [acc]
  (assoc acc :loc (-> acc :intersections peek)))

(defn parse-factory [input]
  (reduce (fn [acc ^Character c]
            (case c
              (\^ \$) acc
              (\N \S \E \W) (letter c acc)
              (\() (open-paren acc)
              (\)) (close-paren acc)
              (\|) (pipe acc)))
          {:factory {}
           :intersections [[0 0]]
           :loc [0 0]}
          input))

(defn build-stack [factory stack]
  (let [nodes (peek stack)
        children (mapcat factory nodes)]
    (if (empty? children)
      stack
      (recur factory (conj stack children)))))

(defn solve-1 []
  (-> (parse-factory input)
      :factory
      (build-stack ['([0 0])])
      count
      dec))

(defn solve-2 []
  (-> (parse-factory input)
      :factory
      (build-stack ['([0 0])])
      (->> (drop 1000))
      (->> (apply concat))
      count))

(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))
