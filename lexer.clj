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
    :else :unknow))

(defn tokenize
  "Transforms a string into a list of tokens. Groups consecutive digits into a single number."
  [expression]
   (loop [chars (seq expression)
          ;;keeps number to build
          current-number nil
          tokens []]
      ;;if nil then end of string 
     (if (nil? chars)
       (if current-number
         ;;grouping consecutive digits seperated with different character into a number for operation
         (conj tokens [:number (Integer/parseInt current-number)])
         tokens)
  
       (let [ch (first chars)
             remaining (rest chars)]
         (cond
           (digit? ch)
           ;;continues loop addding the digit to the number begin created without delivering token yet
           (recur remaining
                  (str current-number ch)
                  tokens)

           ;;concludes number and adds token to operator
           (operator-char? ch)
           (let [new-tokens (if current-number
                              (conj tokens [:number (Integer/parseInt current-number)])
                              tokens)]
             (recur remaining
                    nil
                    (conj new-tokens [:operator ch])))

           ;;same process onto parentheses (adds token) and spaces (does not add token, not necessary for the operations)
           (or (= ch \() (= ch \)))
           (let [new-tokens (if current-number
                              (conj tokens [:number (Integer/parseInt current-number)])
                              tokens)]
             (recur remaining
                    nil
                    (conj new-tokens [:paren ch])))
           (= ch \space)
           (recur remaining
                  nil
                  (if current-number
                    (conj tokens [:number (Integer/parseInt current-number)])
                    tokens))

           ;;
           :else
           (let [new-tokens (if current-number
                              (conj tokens [:number (Integer/parseInt current-number)])
                              tokens)]
             (recur remaining
                    nil
                    (conj new-tokens [:unknown (str ch)]))))))))