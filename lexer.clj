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