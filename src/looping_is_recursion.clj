(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (= k 0)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                   (recur acc (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (not (= (first seq1) (first seq2))) false
                   (and (empty? seq1) (empty? seq2)) true
                   (and (empty? seq1) (not (empty? seq2))) false
                   (and (empty? seq2) (not (empty? seq1))) false
                   :else (recur (rest seq1) (rest seq2))
                   ))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         rest-of a-seq]
    (cond
      (empty? rest-of) nil
      (pred (first rest-of)) index
      :else (recur (inc index) (rest rest-of)))))

(defn avg [a-seq]
  (loop [rest-of a-seq
         curr-sum 0
         index 0]
    (if (empty? rest-of)
      (/ curr-sum index)
      (recur (rest rest-of)
             (+ curr-sum (first rest-of))
             (inc index)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [rest-of a-seq
         result {}]
    (if (empty? rest-of)
      (set (map first (filter (complement empty?) (vals result))))
      (recur (rest rest-of)
             (let [element (first rest-of)]
               (if (contains? result element)
                 (assoc result element (toggle (get result element) element))
                 (assoc result element #{element})))))))

(defn fast-fibo [n]
  (if (or (= n 0) (= n 1))
    n
    (loop [a 1 b 1 idx n]
      (if (= idx 2)
        b
        (recur b (+ a b) (dec idx))))))

(defn cut-at-repetition [a-seq]
  (loop [rest-of a-seq
         result []
         seen #{}]
    (let [next-one (first rest-of)]
      (if (or (empty? rest-of)
              (contains? seen next-one))
        result
        (recur (rest rest-of) (conj result next-one) (conj seen next-one))))))

