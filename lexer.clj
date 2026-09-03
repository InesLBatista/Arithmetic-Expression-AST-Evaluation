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
  "Transforms a string into a list of tokens"
  [expression]
   (loop [chars (seq expression)
          tokens []]
     ;;if nil then end of string 
     (if (nil? chars)
       tokens  
       ;;still characters left
       (let [ch (first chars)
             remaining (rest chars)]
         ;;restart loop only with new values
         (recur remaining
                (conj tokens (char->token ch)))))))