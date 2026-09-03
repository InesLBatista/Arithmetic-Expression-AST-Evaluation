(ns lexer)

(defn digit?
  "Returns true if character is a digit"
  [ch]
  (and (char? ch)
       ;;\0=48 and \9=57 so just needs to be in between
       (<= (int \0) (int ch) (int \9))))

(defn char->token
  "Maps a character to its token type"
  [ch]
  (cond
    (digit? ch) :number
    (= ch \space) :space
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

       ;;remain chacacters to process
       (let [ch (first chars)
             remaining (rest chars)]
         (if (digit? ch)
           ;;new loop with new values 
           (recur remaining
                  (str current-number ch)
                  tokens)
           
           ;;not a digit
           (let [new-tokens (if current-number
                              ;;get the pendent group of digits formed before finding the non digit into a number
                              (conj tokens [:number (Integer/parseInt current-number)])
                              tokens)]
             (recur remaining
                    nil
                    (if (= ch \space)
                      new-tokens
                      ;;for now will not give them a specific type
                      (conj new-tokens [:unknown (str ch)])))))))))