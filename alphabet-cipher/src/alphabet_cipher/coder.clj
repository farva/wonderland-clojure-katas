(ns alphabet-cipher.coder)

(def zval (int \z))
(def aval (int \a))

(defn do-de-en-code [keyword message fn-sum]
  (apply str
         (map
          (fn [k m]
            (char
             (let [sum (+ (int m) (fn-sum k))
                   dist (- sum zval)]
               (if (> dist 0)
                 (dec (+ dist aval))
                 sum))))
          (cycle keyword)
          message)))

(defn encode [keyword message]
  (do-de-en-code keyword message
                 (fn [k]
                   (- (int k) aval))))

(defn decode [keyword message]
  (do-de-en-code keyword message
                 (fn [k]
                   (- (+ zval 1) (int k)))))

(defn decipher [cipher message]
  (let [start (decode message cipher)
        count (count start)]
    (subs
     start
     0
     (loop [pos 1
            offset 0
            anchor nil]
       (if (>= pos count)
         (if anchor
           anchor
           count)
         (let [pointer (get start pos)]
           (if (= pointer
                  (get start offset))
             (recur (inc pos) (inc offset) (if (not anchor) pos anchor))
             (recur (inc pos) 1
                    (if (and
                         (> offset 0)
                         (= (get start 0) pointer))
                      pos
                      anchor)))))))))

(defn shortest-repeating [s]
  (apply
   str
   (get (reduce
         shortest-repeating-reducer
         [(list (first s)) [(first s)] []]
         (next s))
        1)))

(defn shortest-repeating-reducer [[acc res aux] c]
  (if (= c (first acc))
    [(rest acc) res (conj aux c)]
    (let [res (into res aux)]
      (if (= c (first res))
        [(drop 1 (cycle res)) res [c]]
        (let [res (conj res c)]
          [(cycle res) res []])))))

