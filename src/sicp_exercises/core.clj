(ns sicp-exercises.core)

;; EX 1.11
(defn ex1-11a [n]
  "f(n)=n if n<3, f(n)=f(n-1)+2f(n-2)*3f(n-3) if n>=3
  recursive process"
  (if (< n 3)
    n
    (+ (ex1-11a (dec n)) (* 2 (ex1-11a (- n 2))) (* 3 (ex1-11a (- n 3))))))

(defn ex1-11b [n]
  "f(n)=n if n<3, f(n)=f(n-1)+2f(n-2)*3f(n-3) if n>=3
  iterative process"
  (let  [b-iter (fn [[a b c]]
                  [b c (+ c (* 2 b) (* 3 a))])]
    (if (< n 3)
      n
      (loop [cnt 0
             coll [0 1 2]]
        (if (> cnt (- n 3))
          (last coll)
          (recur (inc cnt) (b-iter coll)))))))

;; ex1.12
(def ex1-12
  (fn [n]
    (if (> n 1)
      (map + (cons 0 (ex1-12 (dec n))) (conj (vec (ex1-12 (dec n))) 0))
      [1])))

;;define exponential operator
(defn ** [x n] (reduce * (repeat n x)))

;;SICP ex 1.16 solve iteratively
(defn ex-1-16
  [b n]
  (letfn [(fast-exp [a b n]
            (if (= n 1)
              (* a b)
              (if (even? n)
                (fast-exp a (** b 2) (quot n 2))
                (fast-exp (* a b) b (dec n)))))]
    (fast-exp 1 b n)))
