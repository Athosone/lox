(ns athosone.scanner.token)

(def token-types-set
  #{"(" ")" "{" "}" "," "." "-" "+" ";" "/" "*" "!" "!=" "=" "==" ">" ">=" "<" "<=" "identifier" "string" "number" "and" "class" "else" "false" "fun" "for" "if" "nil" "or" "print" "return" "super" "this" "true" "var" "while" "eof"})
(def token-types
  {:left-paren "("
   :right-paren ")"
   :left-brace "{"
   :right-brace "}"
   :comma ","
   :dot "."
   :minus "-"
   :plus "+"
   :semicolon ";"
   :slash "/"
   :star "*"
   :bang "!"
   :bang-equal "!="
   :equal "="
   :equal-equal "=="
   :greater ">"
   :greater-equal ">="
   :less "<"
   :less-equal "<="
   :identifier "identifier"
   :string "string"
   :number "number"
   :and "and"
   :class "class"
   :else "else"
   :false "false"
   :fun "fun"
   :for "for"
   :if "if"
   :nil "nil"
   :or "or"
   :print "print"
   :return "return"
   :super "super"
   :this "this"
   :true "true"
   :var "var"
   :while "while"
   :eof "eof"})

(defn token [type lexeme literal line]
  {::type type
   ::lexeme lexeme
   ::literal literal
   ::line line})

(comment
  (token :identifier "foo" nil 1))
