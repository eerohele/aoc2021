(ns aoc.solutions
  (:require
   [clojure.java.io :as io])
  (:import
   (java.util ArrayDeque)))

;; https://bsless.github.io/fast-and-elegant-clojure/
(defn sliding
  ([^long n]
   (sliding n 1))
  ([^long n ^long step]
   (fn [rf]
     (let [a (ArrayDeque. n)]
       (fn
         ([]
          (rf))
         ([result]
          (rf result))
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (-> a .toArray vec)]
              (dotimes [_ step] (.removeFirst a))
              (rf result v))
            result)))))))

;; DAY 1

(def lines
  (with-open [reader (io/reader "/tmp/input.txt")]
    (doall (map parse-long (line-seq reader)))))

;; Part 1

(count
  (into []
    (comp
      (sliding 2 1)
      (filter (fn [[a b]] (and b (> b a)))))
    lines))

;; Part 2

(count
  (into []
    (comp
      (sliding 3 1)
      (map (partial reduce + 0))
      (sliding 2 1)
      (filter (fn [[a b]] (and b (> b a)))))
    lines))

;; DAY 2

(def input
  (with-open [reader (io/reader "/tmp/input.txt")]
    (into []
      (comp
        (map #(.split % " "))
        (map (fn [[direction number]]
               [(keyword direction) (parse-long number)])))
      (line-seq reader))))

;; Part 1

(def position
  (reduce
    (fn [position [direction number]]
      (case direction
        :forward (update position :horizontal + number)
        :down (update position :depth + number)
        :up (update position :depth - number)))
    {:horizontal 0 :depth 0}
    input))

(* (:horizontal position) (:depth position))

;; Part 2

(def position
  (reduce
    (fn [{:keys [aim] :as position} [direction number]]
      (case direction
        :forward (-> position (update :horizontal + number) (update :depth + (* aim number)))
        :down (update position :aim + number)
        :up (update position :aim - number)))
    {:horizontal 0 :depth 0 :aim 0}
    input))

(* (:horizontal position) (:depth position))

;; DAY 3

;; Part 1

(def ^:private test-matrix
  [[0 0 1 0 0]
   [1 1 1 1 0]
   [1 0 1 1 0]
   [1 0 1 1 1]
   [1 0 1 0 1]
   [0 1 1 1 1]
   [0 0 1 1 1]
   [1 1 1 0 0]
   [1 0 0 0 0]
   [1 1 0 0 1]
   [0 0 0 1 0]
   [0 1 0 1 0]])

(defn parse-input
  "Given a string path to a file that contains lines of binary numbers, return
  a 2D matrix of the bits in those numbers."
  [path]
  (with-open [reader (-> path io/resource io/reader)]
    (mapv #(mapv (comp parse-long str) %)
      (line-seq reader))))

(comment (take 10 (parse-input "day-3.txt")))

(def bit-matrix (parse-input "day-3.txt"))

(defn rotate
  "Given a 2D matrix, rotate the matrix such that the columns become rows."
  [matrix]
  (apply mapv vector matrix))

(comment
  (rotate [[1 1 0 1 0] [0 1 0 0 1] [1 1 1 1 0]])
  ,)

(defn bit-prevalence
  "Given a comparator and a 2D matrix of bits, return the bit prevalence of
  each row, determined using the comparator."
  [comparator matrix]
  (into
    []
    (comp
      (map frequencies)
      (map #(if (comparator (% 1 0) (% 0 0)) 1 0)))
    matrix))

(def most-frequent-bits (partial bit-prevalence >=))
(def least-frequent-bits (partial bit-prevalence <))

(comment
  (most-frequent-bits (rotate test-matrix))
  (most-frequent-bits [[0 1 0 0] [1 1 0 1] [0 0 1 1]])
  (least-frequent-bits (rotate test-matrix))
  (least-frequent-bits [[0 1 0 0] [1 1 0 1] [0 0 1 1]])
  ,,,)

(defn flip-bits
  "Given a collection of bits, flip each bit."
  [bits]
  (map (partial bit-xor 1) bits))

(comment
  (flip-bits [1 0 1 1 0])
  ,)

(defn binary-long
  "Convert a collection of bits into an unsigned, base-2 long."
  [xs]
  (Long/parseUnsignedLong (apply str xs) 2))

(comment (binary-long [1 0 1 1 0]))

(defn power-consumption
  [bit-matrix]
  (let [γ-bits (most-frequent-bits (rotate bit-matrix))
        ε-bits (flip-bits γ-bits)
        γ-rate (binary-long γ-bits)
        ε-rate (binary-long ε-bits)]
    (* γ-rate ε-rate)))

(power-consumption bit-matrix)

;; Part 2

(defn determine-rating
  ([determine-prevalence rows]
   (determine-rating determine-prevalence rows 0))
  ([determine-prevalence rows n]
   (let [prevalence (-> rows rotate determine-prevalence)]
     (if (= (count rows) 1)
       (-> rows first binary-long)
       (recur
         determine-prevalence
         (filter #(= (nth % n) (nth prevalence n)) rows)
         (inc n))))))

(comment
  (determine-rating most-frequent-bits test-matrix)
  (determine-rating least-frequent-bits test-matrix)
  ,,,)

(defn ratings
  [bit-matrix]
  (let [oxygen-generator-rating (determine-rating most-frequent-bits bit-matrix)
        co2-scrubber-rating (determine-rating least-frequent-bits bit-matrix)]
    (* oxygen-generator-rating co2-scrubber-rating)))

(ratings bit-matrix)
