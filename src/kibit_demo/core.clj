(ns kibit-demo.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic 
             :refer [== run* fresh lvar membero]]
            [clojure.walk :as walk]))


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

(my-rules '(fn [x] (+ x 1)))
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

(my-rules '(fn [x] (+ x 1)))
;; ([(+ x 1) (inc x)])

(defn simplify [expr rules-fn]
  (walk/prewalk-replace (into {} (rules-fn expr)) 
                        expr))

(def expr 
  '(defn my-fun [xs]
     (apply concat 
            (map (fn [x]
                   (into [] (if (< x 0)
                              [(- x 1) (+ x 1)]
                              nil)))
                 xs))))

(simplify expr my-rules)
;; (defn my-fun [xs] 
;;   (mapcat (fn [x] 
;;             (vec (when (neg? x) 
;;                    [(dec x) (inc x)]))) 
;;           xs))
