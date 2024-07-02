(ns athosone.interpreter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as str]))

(defmulti interprete :type)

(defmethod interprete ::ast/literal [{::ast/keys [value]}]
  value)

(defmethod interprete ::ast/grouping [{::ast/keys [expression]}]
  (interprete expression))

(defmethod interprete ::ast/unary [{::ast/keys [operator right]}]
  (let [right (interprete right)]
    (if (= (::token/type operator) ::token/minus)
      (- right)
      right)))

(comment
  (require '[athosone.prettyprinter :refer [pretty-print]]
           '[athosone.parser :as parser :refer [parse]]
           '[athosone.scanner.scan :refer [scan-tokens new-scanner]])
  ;; Operator is a token
  (def a {:type ::ast/literal ::value 1})
  (def b {:type ::ast/grouping ::ast/operator "hello" ::ast/right "world" ::ast/left "www"})
  (pretty-print a)

  (pretty-print (ast/literal "123"))
  (pretty-print (ast/unary (token/token ::token/minus "-" nil 1) (ast/literal "123")))

  (pretty-print (token/token ::token/minus "-" nil 1))

  (pretty-print (ast/binary
                 (ast/unary (token/token ::token/minus "-" nil 1) (ast/literal "123"))
                 (token/token ::token/star "*" nil 1)
                 (ast/grouping (ast/literal "45"))))

  (pretty-print (parse (scan-tokens (new-scanner "(1,2,3)"))))

  ())
