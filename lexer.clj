(ns lexer)

(defn digit?
  "Returns true if character is a digit"
  [ch]
  (and (char? ch)
       ;;\0=48 and \9=57 so just needs to be in between
       (<= (int \0) (int ch) (int \9))))

(def operator-chars #{\+ \- \* \/ \^})

(defn operator-char?
  "Returns true if character is an operator"
  [ch]
  (contains? operator-chars ch))

(defn char->token
  "Maps a character to its token type"
  [ch]
  (cond
    (digit? ch) :number
    (= ch \space) :space
    (operator-char? ch) :operator
    (or (= ch \() (= ch \))) :paren
    :else :unknown))

(defn tokenize
  "Transforms a string into a list of tokens. Groups consecutive digits into a single number."
  [expression]
  (loop [chars (seq expression)
         ;;keeps number to build
         current-number nil
         tokens []]
    ;;if nil then end of string
    (if (empty? chars)
      (if current-number
        ;;grouping consecutive digits seperated with different character into a number for operation
        (conj tokens [:number (Integer/parseInt current-number)])
        tokens)
      (let [ch (first chars)
            remaining (next chars)]
        (cond
          ;;continues loop adding the digit to the number being created without delivering token yet
          (digit? ch)
          (recur remaining
                 (str current-number ch)
                 tokens)

          ;;concludes number and adds token to operator
          (operator-char? ch)
          (recur remaining
                 nil
                 (if current-number
                   (conj (conj tokens [:number (Integer/parseInt current-number)]) [:operator ch])
                   (conj tokens [:operator ch])))

          ;;same process onto parentheses (adds token) and spaces (does not add token, not necessary for the operations)
          (= ch \()
          (recur remaining
                 nil
                 (if current-number
                   (conj (conj tokens [:number (Integer/parseInt current-number)]) [:paren \(])
                   (conj tokens [:paren \(])))

          (= ch \))
          (recur remaining
                 nil
                 (if current-number
                   (conj (conj tokens [:number (Integer/parseInt current-number)]) [:paren \)])
                   (conj tokens [:paren \)])))

          (= ch \space)
          (recur remaining
                 nil
                 (if current-number
                   (conj tokens [:number (Integer/parseInt current-number)])
                   tokens))

          :else
          (recur remaining
                 nil
                 (if current-number
                   (conj (conj tokens [:number (Integer/parseInt current-number)]) [:unknown (str ch)])
                   (conj tokens [:unknown (str ch)]))))))))
