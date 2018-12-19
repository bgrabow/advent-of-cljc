(ns aoc.y2018.d15.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d15.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def starting-hp 200)

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

(def parse-cell {\# :wall
                 \. :space
                 \E :elf
                 \G :gob})

(defn a-star-comparator [a b]
  (compare [(:distance a) (vec (reverse (:loc a)))]
           [(:distance b) (vec (reverse (:loc b)))]))

(defn reading-order [a b]
  (compare (vec (reverse a))
           (vec (reverse b))))

(defn parse-units [parsed-input attack-powers]
  (->> parsed-input
       (filter #(#{:elf :gob} (second %)))
       (map (fn [[loc type]]
              [loc {:type type :hp starting-hp :attack-power (attack-powers type)}]))
       (into (sorted-map-by reading-order))))

(defn parse
  ([input]
   (parse input {:gob 3
                 :elf 3}))
  ([input attack-powers]
   (let [parsed-input (->> (str/split-lines input)
                           (map-indexed-2d (fn [x y c]
                                             [[x y] (parse-cell c)]))
                           (apply concat))
         spaces (->> parsed-input
                     (filter #(#{:space :gob :elf} (second %)))
                     (map first)
                     (into #{}))
         units (parse-units parsed-input attack-powers)]
     {:spaces spaces
      :units  units})))

(defn neighbors [loc]
  (doall (for [v [[1 0] [0 1] [-1 0] [0 -1]]]
           (mapv + loc v))))

(defn make-node [current-node loc]
  {:distance (inc (:distance current-node))
   :loc      loc
   :parent   (:loc current-node)})

(defn origin [node-index node]
  (->> node
       (iterate (comp node-index :parent))
       (drop-while #(< 1 (:distance %)))
       first))

(defn dist-and-origin [node-index node]
  [(:distance node) (origin node-index node)])

(defn dist-and-origin-comparator [[dist-a origin-a] [dist-b origin-b]]
  (compare [dist-a (:distance origin-a) (vec (reverse (:loc origin-a)))]
           [dist-b (:distance origin-b) (vec (reverse (:loc origin-b)))]))

(defn search-space [units spaces initial-loc]
  (let [initial-node {:distance 0 :loc initial-loc}
        open-spaces (apply disj spaces (keys units))]
    (->> {:open-nodes (list initial-node)
          :node-index {(:loc initial-node) initial-node}}
         (iterate (fn [{:keys [open-nodes node-index]}]
                    (let [open-node-pq (sort-by (partial dist-and-origin node-index)
                                                dist-and-origin-comparator
                                                open-nodes)
                          current-node (first open-node-pq)
                          new-nodes (->> current-node
                                         :loc
                                         neighbors
                                         (filter open-spaces)
                                         (remove node-index)
                                         (map (partial make-node current-node)))]
                      {:open-nodes (-> open-node-pq
                                       rest
                                       (into new-nodes))
                       :node-index (merge node-index
                                            (zipmap (map :loc new-nodes)
                                                    new-nodes))})))
         (take-while #(seq (:open-nodes %)))
         (map :node-index))))

(def enemy {:elf :gob
            :gob :elf})

(defn enemy-of? [actor-type [_ other-stats]]
  (let [other-type (:type other-stats)]
    (= other-type
       (enemy actor-type))))

(defn next-location [current-loc initial-units spaces]
  (let [attacking-locations (->> initial-units
                                 (filter (partial enemy-of? (:type (initial-units current-loc))))
                                 keys
                                 (mapcat neighbors)
                                 (into #{})
                                 (filter spaces)
                                 (remove (dissoc initial-units current-loc))
                                 (into #{}))
        searched-space (->> (search-space initial-units spaces current-loc)
                            (filter #(some % attacking-locations))
                            first)]
    (if searched-space
      (->> (keep searched-space attacking-locations)
           (sort-by identity a-star-comparator)
           first
           (iterate (comp searched-space :parent))
           (filter #(or (= current-loc (:loc %))
                        (= current-loc (:parent %))))
           first
           :loc)
      current-loc)))

(defn remove-dead-body [units loc]
  (if (pos? (get-in units [loc :hp]))
    units
    (dissoc units loc)))

(defn attack-order-comparator [[loc-a a] [loc-b b]]
  (compare [(:hp a) (vec (reverse loc-a))]
           [(:hp b) (vec (reverse loc-b))]))

(defn attack-enemy [units current-loc]
  (let [actor-type (get-in units [current-loc :type])]
    (if-let [[target-loc _] (->> (select-keys units (neighbors current-loc))
                                 (filter (partial enemy-of? actor-type))
                                 (sort-by identity attack-order-comparator)
                                 first)]
      (-> units
          (update-in [target-loc :hp] - (get-in units [current-loc :attack-power]))
          (remove-dead-body target-loc))
      units)))

(defn has-type? [type units]
  (not-empty (filter #(= type (:type %))
                     (vals units))))

(defn no-enemies? [acting-unit-loc all-units]
  (when-let [acting-unit (all-units acting-unit-loc)]
    (not (has-type? (enemy (:type acting-unit)) all-units))))

(defn round [spaces m]
  (reduce
    (fn [{:keys [current-units] :as m} [current-loc]]
      (if (no-enemies? current-loc current-units)
        (assoc m :incomplete-round true)
        (if-let [unit-stats (current-units current-loc)]
          (let [next-loc (next-location current-loc current-units spaces)
                new-units (-> current-units
                              (dissoc current-loc)
                              (assoc next-loc unit-stats)
                              (attack-enemy next-loc))]
            (assoc m :current-units new-units))
          m)))
    m
    (:current-units m)))

(defn has-elf? [units]
  (not-empty (filter #(= :elf (:type %))
                     (vals units))))

(defn has-gob? [units]
  (not-empty (filter #(= :gob (:type %))
                     (vals units))))

(defn has-elf-and-goblin? [units]
  (and (has-elf? units)
       (has-gob? units)))

(defn solve-dungeon [dungeon]
  (let [spaces (:spaces dungeon)
        initial-units (:units dungeon)
        iterations (iterate (partial round spaces) {:current-units initial-units})
        [last-round
         {:keys [current-units
                 incomplete-round]}]
        (->> iterations
             (map-indexed #(vector %1 %2))
             (drop-while (comp has-elf-and-goblin? :current-units second))
             first)]
    [(if incomplete-round (dec last-round) last-round) current-units]))

(defn calculate-score [last-round final-units]
  (* last-round
     (->> final-units
          vals
          (map :hp)
          (reduce +))))

(defn n-elves [units]
  (->> units
       vals
       (filter #(= :elf (:type %)))
       count))

(defn all-elves-survive [dungeon elf-ap]
  (let [{:keys [units spaces]} (parse dungeon {:elf elf-ap :gob 3})
        initial-elves (n-elves units)
        final-state (->> {:current-units units}
                         (iterate (partial round spaces))
                         (map-indexed #(vector %1 %2))
                         (take-while #(= initial-elves (n-elves (:current-units (second %)))))
                         (drop-while (comp has-elf-and-goblin? :current-units second))
                         first)]
    final-state))

(defn lowest-ap-all-elves-survive [dungeon]
  (->> (range)
       (map (fn [ap] [ap (all-elves-survive dungeon ap)]))
       (filter second)
       first))

(defn solve-1 []
  (apply calculate-score
         (solve-dungeon (parse my-dungeon))))

(defn solve-2 []
  (let [[_ [last-round {:keys [current-units incomplete-round]}]]
        (lowest-ap-all-elves-survive my-dungeon)]
    (calculate-score (if incomplete-round (dec last-round) last-round)
                     current-units)))

;(deftest part-1
;  (is (= (str answer-1)
;         (str (solve-1)))))
;
;(deftest part-2
;  (is (= (str answer-2)
;         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests)

  (def print-space {:elf   \E
                    :gob   \G
                    :space \.
                    :wall  \#})

  (do
    (let [initial-units {[2 1] {:type :gob, :hp 200}, [3 2] {:type :gob, :hp 200}, [3 4] {:type :gob, :hp 200}, [4 2] {:type :elf, :hp 200}, [5 2] {:type :gob, :hp 200}, [5 3] {:type :gob, :hp 200}, [5 4] {:type :elf, :hp 200}}
          open-spaces #{[2 2] [2 5] [3 3] [5 4] [1 1] [3 4] [4 2] [5 3] [4 1] [5 2] [1 4] [1 3] [1 5] [5 1] [5 5] [2 4] [4 5] [3 1] [2 1] [1 2] [3 5] [3 2]}]
      (is (= [3 1] (next-location [2 1] initial-units open-spaces)))
      (is (= [3 2] (next-location [3 2] initial-units open-spaces)))
      (is (= [3 5] (next-location [3 4] initial-units open-spaces)))
      (is (= [4 2] (next-location [4 2] initial-units open-spaces)))))

  (do
    (let [m (parse my-dungeon)
          spaces (:spaces m)
          initial-units (:units m)]
      (is (= [13 1] (next-location [13 1] initial-units spaces)))))

  (defn print-dungeon [spaces units]
    (let [x-min (dec (apply min (map first spaces)))
          x-max (inc (apply max (map first spaces)))
          y-min (dec (apply min (map second spaces)))
          y-max (inc (apply max (map second spaces)))]
      (->> (for [y (range y-min (inc y-max))]
             (str (->> (for [x (range x-min (inc x-max))]
                         (print-space (or (:type (units [x y]))
                                          (when (spaces [x y]) :space)
                                          :wall)))
                       (apply str))

                  "   "

                  (->> (for [x (range x-min (inc x-max))]
                         (when-let [unit (units [x y])]
                           (str (print-space (:type unit)) \( (:hp unit) \))))
                       (remove nil?)
                       (str/join " "))))
           (str/join \newline))))

  (def test-cases
    [["#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######"
      {:rounds 37
       :value 36334
       :visual "#######\n#...#E#   E(200)\n#E#...#   E(197)\n#.E##.#   E(185)\n#E..#E#   E(200), E(200)\n#.....#\n#######"}]
     ["#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"
      {:rounds 46
       :value 39514
       :visual "#######\n#.E.E.#   E(164), E(197)\n#.#E..#   E(200)\n#E.##.#   E(98)\n#.E.#.#   E(200)\n#...#.#\n#######"}]
     ["#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"
      {:rounds 35
       :value 27755
       :visual "#######\n#G.G#.#   G(200), G(98)\n#.#G..#   G(200)\n#..#..#\n#...#G#   G(95)\n#...G.#   G(200)\n#######"}]])

  (for [[initial {:keys [value visual rounds]}] test-cases]
    (let [{:keys [units spaces]} (parse initial)]
      (println "Initial layout----------------")
      (println (print-dungeon spaces units))
      (println "Combat expected end" rounds)
      (let [[last-round final-units] (solve-dungeon (parse initial))]
        (println "Calculated score" (calculate-score last-round final-units)
                 "Expected" value)
        (println "Expected layout")
        (println visual))))

  (def part-2-test-cases
    [["#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"
      {:rounds 29
       :value 4988
       :ap 15
       :visual "#######\n#..E..#   E(158)\n#...E.#   E(14)\n#.#.#.#\n#...#.#\n#.....#\n#######"}]
     ["#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######"
      {:rounds 33
       :value 31284
       :ap 4
       :visual "#######\n#.E.E.#   E(200), E(23)\n#.#E..#   E(200)\n#E.##E#   E(125), E(200)\n#.E.#.#   E(200)\n#...#.#\n#######"}]
     ["#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######"
      {:rounds 37
       :value 3478
       :ap 15
       :visual "#######\n#.E.#.#   E(8)\n#.#E..#   E(86)\n#..#..#\n#...#.#\n#.....#\n#######"}]
     ["#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"
      {:rounds 39
       :value 6474
       :ap 12
       :visual "#######\n#...E.#   E(14)\n#.#..E#   E(152)\n#.###.#\n#.#.#.#\n#...#.#\n#######"}]
     ["#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"
      {:rounds 30
       :value 1140
       :ap 34
       :visual "#########\n#.......#\n#.E.#...#   E(38)\n#..##...#\n#...##..#\n#...#...#\n#.......#\n#.......#\n#########"}]])

  (for [[dungeon {:keys [rounds value ap visual]}] part-2-test-cases]
    (let [{:keys [spaces units]} (parse dungeon)
          [actual-ap [actual-rounds {:keys [current-units incomplete-round]}]]
          (lowest-ap-all-elves-survive dungeon)]
      (println "-------Initial layout---------")
      (println (print-dungeon spaces units))
      (println "Elves survive with" actual-ap "ap. Expected" ap)
      (println "Done after" actual-rounds "rounds. Expected" rounds)
      (when incomplete-round (println "Incomplete round!"))
      (println (print-dungeon spaces current-units))
      (println "Expected")
      (println visual)
      (println "Final score" (calculate-score (if incomplete-round (dec actual-rounds) actual-rounds)
                                              current-units) "Expected" value))))

