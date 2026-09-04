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
  "Decide whether the operator at the top of the stack should be popped"
  [top-operator current-operator]
  (let [top-prec (operator-precedence top-operator)
        current-prec (operator-precedence current-operator)]
    (and (not= top-operator \()
               (<= current-prec top-prec))))

(defn token->rpn
  "Shunting Yard algothrim"
  [tokens]
  ;;starts with a complete list of tokens, reducing in each iteration. operator-stack keeps temporarily for output to build the result in RPN
  (loop [remaining tokens
         operator-stack []
         output []]
    (if (empty? remaining)
      ;;when the tokens end, function combines the current output with the remaining operators from the operator stack, but reverses the stack so that the top element comes out first
      (concat output (reverse operator-stack))

      (let [token (first remaining)
            rest-tokens (rest remaining)]
        ;;token is a number, addind it to the end of output
        (cond
          (models/numeric-token? token)
          (recur rest-tokens
                 operator-stack
                 (conj output token))
          
          ;;opening parenthesis, stacks
          (= token [:paren \(])
          (recur rest-tokens
                 (conj operator-stack token)
                 output)
          
          ;;closing parenthesis, pops until finding the opening one 
          (= token [:paren \)])
          ;;it traverses the inverted stack from top to bottom, removing all operators (take-while) until it finds the parenthesis (
          (let [popped (take-while #(not= % [:paren \(]) (reverse operator-stack))
                remaining-stack (drop (count popped) operator-stack)
                ;;removes the opening from the stack
                final-stack (rest remaining-stack)]
            (recur rest-tokens
                   final-stack
                   (concat output popped)))
          
          ;;to extract from the stack the item in it has to be an operator and needs to have higher or equal precedence to the current operator 
          (is-operator? token)
          (let [current-op (second token)
                pop-condition (fn [stack-op]
                                (and (is-operator? stack-op)
                                     (should-pop-operator? (second stack-op) current-op)))]
            ;;if stack is empty, keeps the new operator in the top
            (if (empty? operator-stack)
              (recur rest-tokens
                     (conj operator-stack token)
                     output)
              ;;not being empty, removes from stack's top the pop-condition's operators. adds the operators from output, puts the current operator in the top of remaining-stack
              (let [popped (take-while pop-condition (reverse operator-stack))
                    remaining-stack (drop (count popped) operator-stack)]
                (recur rest-tokens
                       (conj remaining-stack token)
                       (concat output popped)))))
          :else
          (recur rest-tokens
                 operator-stack
                 output))))))