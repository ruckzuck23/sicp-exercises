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

;;ex 1.16 solve iteratively
(defn ex-1-16
  [b n]
  (letfn [(fast-exp [a b n]
            (if (= n 1)
              (* a b)
              (if (even? n)
                (fast-exp a (** b 2) (quot n 2))
                (fast-exp (* a b) b (dec n)))))]
    (fast-exp 1 b n)))

;;ex1.17
;;implement an algorithm faster than the following one, using my-double and my-halve
;(define (* a b)
;  (if (= b 0)
;      0
;      (+ a (* a (- b 1)))))
(defn my-double
        [a]
        (+ a a))

(defn my-halve
        "Takes only even numbers. Divide inputs by two"
        [a]
        (if (even? a)
          (quot a 2)
          (System/exit 0)))


(defn ex1-17
  [a b]
  (if (= b 0)
      0
      (if (even? b)
          (my-double (ex1-17 a (my-halve b)))
          (+ a (my-double (ex1-17 a (my-halve (dec b))))))))

;;ex1.18
;;implement an iterative version of ex1.17 solution
(defn fast-*
  [a b]
  (letfn [(iter [acc a b]
                 (cond (= b 0) acc
                       (even? b) (iter acc (my-double a) (my-halve b))
                       :else (iter (+ acc a) (my-double a) (my-halve (dec b)))))]
    (iter 0 a b)))