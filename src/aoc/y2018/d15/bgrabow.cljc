(ns aoc.y2018.d15.bgrabow
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    ;[aoc.y2018.d15.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]))

(def test-dungeon
  ;#######
  ;#.G...#   G(200)
  ;#...EG#   E(200), G(200)
  ;#.#.#G#   G(200)
  ;#..G#E#   G(200), E(200)
  ;#.....#
  ;#######
  "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######")

(def my-dungeon
  "################################\n#########....G#######.##########\n##########.G########...#########\n###########.########.#.#########\n###########.#..G######..######.#\n##########..#...###......G..##.#\n##.#######......#..G....E#.....#\n##.##..######...........E..E####\n##.##...###................#####\n#.....G.G...........G.....######\n#...G....G...................###\n#G.G.............EG..........###\n#..G..........#####.........####\n##.G.......G.#######.........###\n#####....G..#########..G.E....##\n####....#...#########.........##\n#######.##..#########...#.....##\n#########...#########.....######\n##########..#########G....######\n##########...#######..E...######\n##########....#####...#EE.######\n#########.........G.......######\n#########............###########\n############..........##########\n############.......#.###########\n###########...........##########\n###########........#.###########\n##########E.#......#############\n#########...##....E.############\n#########.######....############\n################....############\n################################\n")

(def test-dungeon2
  "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########")

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

(map println (str/split-lines test-dungeon))

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

(defn search-space [units spaces initial-loc]
  (let [initial-node {:distance 0 :loc initial-loc}
        open-spaces (apply disj spaces (keys units))]
    (->> {:open-node-pq (sorted-set-by a-star-comparator initial-node)
          :node-index   {(:loc initial-node) initial-node}}
         (iterate (fn [{:keys [open-node-pq node-index]}]
                    (let [current-node (first open-node-pq)
                          new-nodes (->> current-node
                                         :loc
                                         neighbors
                                         (filter open-spaces)
                                         (remove node-index)
                                         (map (partial make-node current-node)))]
                      {:open-node-pq (-> open-node-pq
                                         (disj current-node)
                                         (into new-nodes))
                       :node-index   (merge node-index
                                            (zipmap (map :loc new-nodes)
                                                    new-nodes))})))
         (take-while #(seq (:open-node-pq %)))
         (map :node-index))
    #_(loop [open-node-pq (sorted-set-by a-star-comparator initial-node)
             node-index {(:loc initial-node) initial-node}]
        (if (empty? open-node-pq)
          node-index
          (let [current-node (first open-node-pq)
                new-nodes (->> current-node
                               :loc
                               neighbors
                               (filter open-spaces)
                               (remove node-index)
                               (map (partial make-node current-node)))]
            (recur (-> open-node-pq
                       (disj current-node)
                       (into new-nodes))
                   (merge node-index
                          (zipmap (map :loc new-nodes)
                                  new-nodes))))))))

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

(defn remove-dead-body [units loc]
  (if (pos? (get-in units [loc :hp]))
    units
    (dissoc units loc)))

(def print-space {:elf   \E
                  :gob   \G
                  :space \.
                  :wall  \#})

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

(defn attack-order [[loc-a a] [loc-b b]]
  (compare [(:hp a) (vec (reverse loc-a))]
           [(:hp b) (vec (reverse loc-b))]))

(defn attack-enemy [units current-loc]
  (let [actor-type (get-in units [current-loc :type])]
    (if-let [[target-loc _] (->> (select-keys units (neighbors current-loc))
                                 (filter (partial enemy-of? actor-type))
                                 (sort-by identity attack-order)
                                 first)]
      (-> units
          (update-in [target-loc :hp] - (get-in units [current-loc :attack-power]))
          (remove-dead-body target-loc))
      units)))

(defn println-identity [& args]
  (apply println args)
  (last args))

(defn step [spaces units]
  ;(println "Starting a round...")
  (reduce (fn [current-units [current-loc]]
            ;(println "Acting unit: " current-loc (current-units current-loc))
            (if-let [unit-stats (current-units current-loc)]
              (let [next-loc (next-location current-loc current-units spaces)]
                (-> current-units
                    (dissoc current-loc)
                    (assoc next-loc unit-stats)
                    (attack-enemy next-loc)))
              current-units))
          units
          units))

(defn has-elf? [units]
  (not-empty (filter #(= :elf (:type %))
                     (vals units))))

(defn has-gob? [units]
  (not-empty (filter #(= :gob (:type %))
                     (vals units))))

(defn has-elf-and-goblin? [units]
  (and (has-elf? units)
       (has-gob? units)))

(let [m (parse my-dungeon)
      spaces (:spaces m)
      initial-units (:units m)
      iterations (iterate (partial step spaces) initial-units)]
  (doseq [units (take 5 (drop 200 iterations))]
    (println (print-dungeon spaces units))))

(defn solve-dungeon [dungeon]
  (let [m (parse dungeon)
        spaces (:spaces m)
        initial-units (:units m)
        iterations (iterate (partial step spaces) initial-units)
        [last-round final-units] (first (drop-while (comp has-elf-and-goblin? second)
                                                    (map-indexed #(vector %1 %2) iterations)))]
    (println "Combat ends on" last-round)
    (println (print-dungeon spaces final-units))
    [last-round final-units]))

(defn calculate-score [last-round final-units]
  (* (dec last-round)
     (->> final-units
          vals
          (map :hp)
          (reduce +))))

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
    (let [[last-round final-units] (solve-dungeon initial)]
      (println "Calculated score" (calculate-score last-round final-units)
               "Expected" value)
      (println "Expected layout")
      (println visual))))

(defn solve-1 []
  (apply calculate-score (solve-dungeon my-dungeon)))
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

