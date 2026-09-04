(ns core
  (:require [lexer :as lexer]
             [parser :as parser]
             [evaluator :as evaluator]))

(defn evaluate
  "Evaluates an arithmetic expression. Lexer + Parser + Evaluator."
  [expression]
  (println "Input:" expression)
   (let [tokens (lexer/tokenize expression)
         _ (println "Tokens:" tokens)
         rpn (parser/parse tokens)
         _ (println "RPN:" rpn)
         result (evaluator/evaluate rpn)]
     (println "Result:" result)
     result))

(defn -main
  "Entry point from command line"
  [& args]
  (if (seq args)
    (evaluate (first args))
    (println "Usage: clj -M -m core \"2 + 3\"")))