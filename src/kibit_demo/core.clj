(ns kibit-demo.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic 
             :refer [== run* fresh lvar membero]]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]))


(run* [q]
  (== 42 q))
;; (42)

(run* [q]
  (== q 42))
;; (42)

(run* [q]
  (== q 42)
  (== 42 q))
;; (42)

(run* [q]
  (== q 42)
  (== 41 q))
;; ()

(run* [q]
  (== q '(+ 42 1)))
;; ((+ 42 1))

(run* [q]
  (== `(+ ~q 1) 
      `(+ 42 1)))
;; (42)

(run* [q]
  (fresh [x]
    (== `(+ ~x 1) 
        `(+ 42 1))
    (== x q)))
;; (42)

(run* [q]
  (fresh [x]
    (== `(+ ~x 1) 
        `(+ 42 1))
    (== `(inc ~x) q)))
;; ((clojure.core/inc 42))


(defn inc-rule [expr]
  (run* [q]
    (fresh [x]
      (== `(+ ~x 1) expr)
      (== `(inc ~x) q))))

(inc-rule `(+ 42 1))
;; ((clojure.core/inc 42))

(inc-rule `(map inc [1 2 3]))
;; ()

(defn rule [[patt subst]]
  (fn [expr]
    (run* [q]
      (== patt expr)
      (== subst q))))

((rule [`(+ ~(lvar 'x false) 1) 
        `(inc ~(lvar 'x false))])
 `(+ 42 1))
;; ((clojure.core/inc 42))

(let [coll (lvar 'coll false)
      keys (lvar 'keys false)
      val  (lvar 'val  false)
      update-in-rule (rule [`(update-in ~coll ~keys assoc ~val)
                            `(assoc-in ~coll ~keys ~val)])
      expression `(update-in {:x {:y 0}} [:x :z] assoc 0)]
  (update-in-rule expression))
;; ((clojure.core/assoc-in {:x {:y 0}} [:x :z] 0))

(defn prepare [form]
  (walk/prewalk
   #(if (and (symbol? %)
             (.startsWith (name %) "?"))
      (lvar % false)
      %)
   form))

(prepare '[(+ ?x 1) (inc ?x)])
;; [(+ <lvar:?x> 1) (inc <lvar:?x>)]

((rule (prepare '[(+ ?x 1) (inc ?x)]))
 '(+ x 1))
;; ((inc x))

((rule (prepare '[(update-in ?coll ?keys assoc ?val) 
                  (assoc-in ?coll ?keys ?val)]))
 '(update-in coll [:x :y] assoc 42))
;; ((assoc-in coll [:x :y] 42))


;; Rule version 2

(defn rule [r]
  (let [r (prepare r)]
    (fn [expr]  
      (run* [q]
        (fresh [patt subst]
          (== [patt subst] r)
          (== patt expr)
          (== [expr subst] q))))))

((rule '[(+ ?x 1) (inc ?x)])
 '(+ 42 1))
;; ([(+ 42 1) (inc 42)])


;; Rules vesion 1:
;; Meet membero

(run* [q]
  (membero q [1 2 3 4]))
;; (1 2 3 4)


(run* [q]
  (membero 2 [1 q 3 4]))
;; (2)

(run* [q]
  (fresh [x y]  
    (membero x [1 2 3 4])
    (membero y [3 4 5 6])
    (== q [x y])))
;; ([1 3] [1 4] [2 3] [1 5] [1 6] 
;;  [2 4] [3 3] [2 5] [2 6] [3 4] 
;;  [4 3] [3 5] [3 6] [4 4] [4 5] 
;;  [4 6])

(run* [q]
  (fresh [x y]
    (== x y)
    (membero x [1 2 3 4])
    (membero y [1 2 3 4])
    (== q [x y])))
;; ([1 1] [2 2] [3 3] [4 4])

(defn rules [rs]
  (let [rs (map prepare rs)]
    (fn [expr]
      (run* [q]
        (fresh [patt subst]
          (membero [patt subst] rs)
          (== patt expr)
          (== [expr subst] q))))))

(defmacro defrules
  [name & rs]
  `(def ~name
     (rules (quote ~rs))))

(defrules my-rules
  [(+ ?x 1) (inc ?x)]
  [(- ?x 1) (dec ?x)]
  [(< 0 ?x) (pos? ?x)]
  [(< ?x 0) (neg? ?x)]
  [(if ?test ?else nil) (when ?test ?else)]
  [(into [] ?coll) (vec ?coll)]
  [(apply concat (map ?f ?xs)) (mapcat ?f ?xs)])

(my-rules '(if (some f xs) f nil))
;; ([(if (some f xs) f nil) (when (some f xs) f)])

(my-rules '(fn [x] 
             (if (< x 0) 
               (+ x 1)
               nil)))
;; []

;; Rules version 2

(tree-seq sequential? seq '(map (fn [x y] (+ x y)) xs))
;; ((map (fn [x y] (+ x y)) xs), 
;;  map, (fn [x y] (+ x y)), fn, 
;;  [x y], x, y, (+ x y), +, x 
;;  y, xs)

(defn rules [rs]
  (let [rs (map prepare rs)]
    (fn [expr]
      (run* [q]
        (fresh [patt subst e]
          (membero [patt subst] rs)
          (membero e (tree-seq sequential? seq expr))
          (== patt e)
          (== [e subst] q))))))

(my-rules '(fn [x] 
             (if (< x 0) 
               (+ x 1)
               nil)))
;; ([(+ x 1) (inc x)] 
;;  [(< x 0) (neg? x)] 
;;  [(if (< x 0) (+ x 1) nil) (when (< x 0) (+ x 1))])

(defn simplify-one [term rules-fn]
  (walk/prewalk-replace (into {} (rules-fn term))
                        term))

(defn simplify [term rules-fn]
  (loop [term term]
    (let [sterm (simplify-one term rules-fn)]
      (if (= sterm term)
        sterm
        (recur sterm)))))

(simplify '(fn [x] 
             (if (< x 0) 
               (+ x 1)
               nil))
          my-rules)
;; (fn [x] 
;;   (when (neg? x) 
;;     (inc x)))

(simplify '(defn my-fun [xs]
             (apply concat 
                    (map (fn [x]
                           (into [] (if (< x 0)
                                      [(- x 1) (+ x 1)]
                                      nil)))
                         xs)))
          my-rules)
;; (defn my-fun [xs] 
;;   (mapcat (fn [x] 
;;             (vec (when (neg? x) 
;;                    [(dec x) (inc x)]))) 
;;           xs))


;; http://www.cs.tau.ac.il/~nachumd/rewrite/97/notes97.pdf
;; http://www.cs.tau.ac.il/~nachum/papers/taste-fixed.pdf
(defrules insertion-sort
  [(max z ?x) ?x]
  [(max ?x z) ?x]
  [(max (s ?x) (s ?y)) (s (max ?x ?y))]
  [(min z ?x) z]
  [(min ?x z) z]
  [(min (s ?x) (s ?y)) (s (min ?x ?y))]
  [(sort nil) nil]
  [(sort (cons ?x ?y)) (insert ?x (sort ?y))]
  [(insert ?x nil) (cons ?x nil)]
  [(insert ?x (cons ?y ?z)) (cons (min ?x ?y) (insert (max ?x ?y) ?z))])

(simplify '(sort (cons (s (s z)) 
                       (cons (s z) 
                             (cons (s (s (s z))) 
                                   (cons z nil)))))
          insertion-sort)
;; (cons z (cons (s z) (cons (s (s z)) (cons (s (s (s z))) nil))))


(defn to-peano [n]
  (if (zero? n) 'z (list 's (to-peano (dec n)))))

(defn from-peano [s]
  (if (= s 'z) 0 (inc (from-peano (second s)))))

(from-peano (to-peano 10))

(defn to-cons [coll]
  (when coll (list 'cons (first coll) (to-cons (next coll)))))

(defn from-cons [c]
  (when c (cons (second c) (from-cons (nth c 2)))))

(from-cons (to-cons [1 2 3]))

(defn encode [coll]
  (to-cons (map to-peano coll)))

(defn decode [c]
  (map from-peano (from-cons c)))

(decode (encode [5 3 1 2 4]))

(defn isort [coll]
  (let [term (list 'sort (encode coll))]
    (-> term (simplify insertion-sort) decode)))

(isort [3 1 4 2])
