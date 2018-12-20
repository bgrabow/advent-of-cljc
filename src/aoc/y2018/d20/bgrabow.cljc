(ns aoc.y2018.d20.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d20.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

"^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
"WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"
"(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))"

(defn parse [input]
  (-> input
      (str/replace #"\^" "(")
      (str/replace #"\$" ")")
      (str/replace #"\|" " ")
      (str/replace #"\b" "\"")
      u/read-string))

(parse "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")

(->> (u/read-string "(WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS)))))")
     second)

(->> (parse "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
     second)

["WSSEESWWWNW" ["S"] ["NENNEEEENN" ["ESSSSW" ["NWSW" "SSEN"]]]]

"^ENWWW(NEEE|SSE(EE|N))$"

"^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"

(def backtrack {\N \S
                \S \N
                \E \W
                \W \E})

(defn not-backtracking [[x y]]
  (not= (backtrack x) y))

(defn path-length [s]
  (if-let [s (->> (partition 2 1 s)
                  (take-while not-backtracking))]
    (inc (count s))
    0))

(deftest linear
         (is (= (path-length "WSSEESWWWNW") 11)))
(deftest backtrack
         (is (= (path-length "NEWS") 2)))
(deftest backtrack
         (is (= (path-length "SSEN") 4)))




(defn solve-1 [])
;; TODO


(defn solve-2 [])
;; TODO


(deftest part-1
         (is (= (str answer-1)
                (str (solve-1)))))

(deftest part-2
         (is (= (str answer-2)
                (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

