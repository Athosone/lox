(ns athosone.scanner.token)

(def reserved-words
  #{"and" "class" "else" "false" "for" "fun" "if" "nil" "or" "print" "return" "super" "this" "true" "var" "while"})

(defn token [type lexeme literal line]
  {::type type
   ::lexeme lexeme
   ::literal literal
   ::line line})

(defn keywordize-token [t]
  (keyword "athosone.scanner.token" t))

(comment
  (keywordize-token "for")
  (token :identifier "foo" nil 1))
