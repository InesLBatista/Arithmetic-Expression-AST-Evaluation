(ns models)

(defn numeric-token?
  "Returns true if token is a number"
  [token]
  ;;tokens are represented as vectors [:type value]
  (and (sequential? token)
       ;;checking its :type here with just the first digit
       (= (first token) :number)))