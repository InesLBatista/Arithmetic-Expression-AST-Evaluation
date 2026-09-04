(ns evaluator
  (:require [models :as models]))

(def operator-fn
  {\+ + 
   \- - 
   \* * 
   \/ / 
   \^ #(Math/pow %1 %2)})

(defn build-ast
  "Builds the Abstract Syntax Tree from the RPN expression"
  [rpn]
  (loop [remaining rpn
         stack []]
    (if (empty? remaining)
      ;;in the end the stack should only have one item, the complete ast
      (first stack)
      
      (let [token (first remaining)
            rest-tokens (rest remaining)]
        (if (models/numeric-token? token)
          ;;if number then stack as a left node
          (recur rest-tokens
                 (conj stack token))
          
          (let [right (peek stack)
                left (peek (pop stack))
                new-stack (pop (pop stack))]
            (recur rest-tokens
                   (conj new-stack [:operator (second token) left right]))))))))

(defn eval-ast
  "Recursively checks the AST"
  [ast]
  (if (models/numeric-token? ast)
    (second ast)
    
    (let [[_ op left right] ast
          left-val (eval-ast left)
          right-val (eval-ast right)
          fn (get operator-fn op)]
      (fn left-val right-val))))

(defn evaluate
  "Evaluates the RPN expression"
  [rpn]
  (let [ast (build-ast rpn)]
    (eval-ast ast)))