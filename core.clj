(ns core
  (:require [lexer :as lexer]))

(defn evaluate
  "Evaluates an arithmetic expression. Just tokenizes and shows tokens for now"
  [expression]
  (println "Input:" expression)
  (let [tokens (lexer/tokenize expression)]
    (println "Tokens:"  tokens)
    tokens))

(defn -main
  "Entry point from command line"
  [& args]
  (if (seq args)
    (evaluate (first args))
    (println "Usage: clj -M -m core \"2 + 3\"")))