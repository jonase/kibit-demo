(ns kibit-demo.quiz
  (:require [clojure.core.logic.unifier :refer [unify]]))

(unify '[?x 42])
;; 42

(unify '[(+ 42 1) (+ ?x 1)])
;; (+ 42 1)

(unify '[(f ?x ?y) 
         (f (g ?y a) (h a))])
;; (f (g (h a) a) (h a))

(unify '[[foo ?x ?z] 
         [?y  [baz ?y] {:bar ?x}]])
;; [foo [baz foo] {:bar [baz foo]}]

;; Fill in the blanks :)
(defn rule [[pat sbst]]
  (fn [expr]
    (unify [___ ___])))

;; such that
((rule '[(+ ?x 1) (inc ?x)]) '(+ 42 1))
;; [(+ 42 1) (inc 42)]

