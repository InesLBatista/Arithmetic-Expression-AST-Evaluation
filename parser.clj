(ns parser
  (:require [models :as models]))

(defn peek-stack
  "Returns the last item of the stack without removing it."
  [stack]
  (last stack))

(defn operator-precedence
  "Returns an operator's precedence"
  [operator]
  (get models/precedence operator 0))

(defn is-operator?
  "Checks if a certain token is as known operator"
  [token]
  (and (models/operator? token)
       (> (operator-precedence (second token)) 0)))


(defn should-pop-operator?
  [top-operator current-operator]
  (let [top-prec (operator-precedence top-operator)
        current-prec (operator-precedence current-operator)]
    (and (not= top-operator \()
               (<= current-prec top-prec))))