(ns aoc.y2018.d15.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d15.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def dungeon
  ;#######
  ;#.G...#   G(200)
  ;#...EG#   E(200), G(200)
  ;#.#.#G#   G(200)
  ;#..G#E#   G(200), E(200)
  ;#.....#
  ;#######
  "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")

(def static-unit-stats {:attack-power 3})
(def starting-unit-stats {:hit-points 200})

; Round structure
; * Pick the first unit sorted by [x y]
; ** If no attack targets adjacent, unit moves towards closest target
;    by A* path distance.
; *** Target space ties broken by [x y] sort on target spaces
; *** Ties between equivalent movement options towards target
;     square broken by [x y] sort on immediate movement destinations
; ** Unit attacks enemy in range.
; *** Ties between possible targets broken first by lowest HP, then by
;     [x y] sort on target position

(defn map-indexed-2d [f colls]
  (map-indexed
    (fn [y coll]
      (map-indexed
        (fn [x e]
          (f x y e))
        coll))
    colls))

(map println (str/split-lines dungeon))

(def parse-cell {\# :wall
                 \. :space
                 \E :elf
                 \G :goblin})

(def new-goblin starting-unit-stats)
(def new-elf starting-unit-stats)

(defn parse-units [parsed-input type new-unit]
  (->> parsed-input
       (filter #(#{type} (second %)))
       (map first)
       (#(zipmap % (repeat new-unit)))
       (into (sorted-map))))

(defn parse [input]
  (let [parsed-input (->> (str/split-lines input)
                          (map-indexed-2d (fn [x y c]
                                            [[x y] (parse-cell c)]))
                          (apply concat))
        dungeon (->> parsed-input
                     (filter #(#{:space :goblin :elf} (second %)))
                     (map first)
                     (into #{}))
        goblins (parse-units parsed-input :goblin new-goblin)
        elves (parse-units parsed-input :elf new-elf)]
    {:dungeon dungeon
     :goblins goblins
     :elves elves}))

(defn solve-1 [])
;; TODO


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

