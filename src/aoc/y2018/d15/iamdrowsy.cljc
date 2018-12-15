(ns aoc.y2018.d15.iamdrowsy
  (:refer-clojure :exclude [read-string format])
  (:require
    [aoc.utils :as u :refer [deftest read-string format]]
    [aoc.y2018.d15.data :refer [input answer-1 answer-2]]
    [clojure.test :as t :refer [is testing]]
    [clojure.string :as str]
    [com.rpl.specter :refer [setval MAP-VALS not-selected? path pred= select NONE selected? select-one]]
    [clojure.set :as set]))

(def my-pmap
  #?(:clj pmap
     :cljs map))

(defn parse-line [line-num line]
  (into {} (map-indexed (fn [i c] [[line-num i] c]) line)))

(defn parse-input [input]
  (apply merge (map-indexed parse-line (str/split-lines input))))

(defn extract-board [parsed-input]
  (setval [MAP-VALS #{\G \E}] \. parsed-input))

(defn extract-players [parsed-input]
  (->> parsed-input
       (setval [MAP-VALS (not-selected? #{\E \G})] NONE)
       (into {} (map-indexed (fn [i [coords player-char]]
                               [i {:coords coords
                                   :type player-char
                                   :hp 200
                                   :ap 3
                                   :id i}])))))

(defn draw-line [board max-x line-num]
  (str/join (map #(get board [line-num %]) (range (inc max-x)))))

(defn merge-board [board players]
  (let [player-type-by-coords (zipmap (map :coords (vals players))
                                      (map :type (vals players)))]
    (merge board player-type-by-coords)))

(defn draw-board [board]
  (let [max-x (apply max (map first (keys board)))
        max-y (apply max (map second (keys board)))]
    (str/join "\n" (map (partial draw-line board max-y) (range (inc max-x))))))

(defn adjecent-coords [[x y]]
  #{[(dec x) y] [(inc x) y] [x (inc y)] [x (dec y)]})

(defn next-adjecent [coords]
  (apply set/union (map adjecent-coords coords)))

(defn enemies [current-type]
  (path MAP-VALS (not-selected? :type (pred= current-type))))

(defn targets [players current-player]
  (let [current-type (:type (players current-player))
        enemy-pos (select [(enemies current-type) :coords] players)]
    (next-adjecent enemy-pos)))

(def BLOCKED
  (path MAP-VALS #{\E \G \#}))

(defn find-nearest-reachable-target [walkable-coords targets start-coord]
  (cond (not (walkable-coords start-coord)) nil
        (targets start-coord) [0 start-coord start-coord]
        :else
        (loop [steps 1
               walked-coords #{start-coord}]
          (let [new-walked (set/union walked-coords
                                      (set/intersection (next-adjecent walked-coords) walkable-coords))]
            (cond (= new-walked walked-coords) nil

                  (not (empty? (set/intersection new-walked targets)))
                  [steps (vec (first (sort (set/intersection new-walked targets)))) start-coord]

                  :else (recur (inc steps) new-walked))))))

(defn find-next-step [board players current-player]
  (let [merged-board (merge-board board players)
        t (targets players current-player)
        walkable-coords (into #{} (keys (setval BLOCKED NONE merged-board)))]
    (->> (next-adjecent #{(:coords (players current-player))})
         (map (partial find-nearest-reachable-target walkable-coords t))
         (remove nil?)
         (sort)
         (first)
         (last))))

(defn still-alive? [players current-player]
  (players current-player))

(defn move [players board current-player]
  (if-let [next-coord (find-next-step board players current-player)]
    (setval [current-player :coords]
            next-coord
            players)
    players))

(defn lowest-hitpoints-coords [players candidate-coords]
  (second (first (sort (map (fn [x] [(:hp x) (:coords x)])
                            (select [MAP-VALS (selected? :coords candidate-coords)] players))))))


(defn find-attack-coords [players current-player]
  (let [current-type (:type (players current-player))
        enemy-coords (into #{} (select [(enemies current-type) :coords] players))]
    (lowest-hitpoints-coords players
                             (set/intersection enemy-coords (next-adjecent #{(:coords (players current-player))})))))

(defn attack [players current-player]
  (if-let [attack-coords (find-attack-coords players current-player)]
    (let [attack-target-id (select-one [MAP-VALS (selected? :coords (pred= attack-coords)) :id] players)
          current-ap (:ap (players current-player))
          target-hp (:hp (players attack-target-id))]
      (if (<= target-hp current-ap)
        (dissoc players attack-target-id)
        (update-in players [attack-target-id :hp] #(- % current-ap))))
    players))

(defn make-move [board players current-player]
  (if (still-alive? players current-player)
    (cond-> players
            (not (find-attack-coords players current-player)) (move board current-player)
            :true (attack current-player))
    players))

(defn make-turn [board players]
  (let [player-order (map first (sort-by #(:coords (val %)) players))]
    (reduce (partial make-move board)
            players player-order)))

(defn done? [players]
  (= 1 (count (into #{} (select [MAP-VALS :type] players)))))

(defn sum-hp [players]
  (reduce + (select [MAP-VALS :hp] players)))

(def ELVES
  (path MAP-VALS (selected? :type (pred= \E))))

(defn elf-lost? [players elf-count]
  (not= (count (select ELVES players))
        elf-count))

(defn run-game [input no-loss? elf-power]
  (let [parsed-input (parse-input input)
        board (extract-board parsed-input)
        players (setval [ELVES :ap] elf-power
                        (extract-players parsed-input))
        elf-count (count (select ELVES players))]
    (loop [turn 0
           players players]
      #_(println "Turn" turn)
      #_(println (draw-board (merge-board board players)))
      (cond (and no-loss? (elf-lost? players elf-count)) (do #_(println "failed with " elf-power) :failed)
            (done? players) (* (dec turn) (sum-hp players))
            :else (recur (inc turn) (make-turn board players))))))

(defn solve-1 []
  (run-game input false 3))

(defn solve-2 []
  (first (drop-while #(= :failed %)
                     (my-pmap (partial run-game input true)
                              (drop 3 (range))))))


(deftest ^:slow part-1
  (is (= (str answer-1)
         (str (solve-1)))))

(deftest ^:slow part-2
  (is (= (str answer-2)
         (str (solve-2)))))

;;;; Scratch

(comment
  (t/run-tests))

