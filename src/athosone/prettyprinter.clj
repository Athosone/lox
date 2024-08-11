(ns athosone.prettyprinter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as str]))

(defmulti pretty-print :type)

(defmethod pretty-print :default []
  (println))

(defn parentesize [name & expressions]
  (str "(" name " " (map pretty-print expressions) ")"))

(defmethod pretty-print ::ast/unary [{::ast/keys [operator right]}]
  (str "(" (::token/lexeme operator) " " (pretty-print right) ")"))

(defmethod pretty-print ::ast/binary [{::ast/keys [lhs operator rhs]}]
  (str "(" (::token/lexeme operator) " " (pretty-print lhs) " " (pretty-print rhs) ")"))

(defmethod pretty-print ::ast/grouping [{::ast/keys [expression]}]
  (str "(" "group " (pretty-print expression) ")"))

(defmethod pretty-print ::ast/literal [{::ast/keys [value]}]
  (if (nil? value) "nil" (str/trim (str value))))

(comment
  ;; Operator is a token
  (def a {:type ::ast/literal ::ast/operator "hello" ::ast/right "world"})
  (def b {:type ::ast/grouping ::ast/operator "hello" ::ast/right "world" ::ast/left "www"})
  (pretty-print a)

  (pretty-print (ast/literal "123"))
  (pretty-print (ast/unary (token/token ::token/minus "-" nil 1) (ast/literal "123")))

  (pretty-print (token/token ::token/minus "-" nil 1))

  (pretty-print (ast/binary
                 (ast/unary (token/token ::token/minus "-" nil 1) (ast/literal "123"))
                 (token/token ::token/star "*" nil 1)
                 (ast/grouping (ast/literal "45"))))

  ())
