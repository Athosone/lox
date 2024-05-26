(ns athosone.prettyprinter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as string]))

(defmulti pretty-print :type)

(defmethod pretty-print ::ast/unary [{::ast/keys [operator right]}]
  (parentesize (::token/text operator) right))

(defmethod pretty-print ::ast/binary [{::ast/keys [lhs operator rhs]}]
  (parentesize (::token/text operator) (str lhs " " operator " " rhs)))

(defmethod pretty-print ::ast/grouping [{::ast/keys [expression]}]
  (parentesize "group" expression))

(defmethod pretty-print ::ast/literal [{::ast/keys [value]}]
  (parentesize value))

(defmethod pretty-print ::ast/expression [{::ast/keys [value]}]
  (parentesize value))

(defn parentesize [name & expressions]
  (str "(" name " " (map #(pretty-print %) expressions) ")"))

(defprotocol PrettyPrintable
  (pprint [this]))

(comment
  ;; Operator is a token
  (def a {:type ::ast/unary ::ast/operator "hello" ::ast/right "world"})
  (def b {:type ::ast/grouping ::ast/operator "hello" ::ast/right "world" ::ast/left "www"})
  (pretty-print a)
  (pretty-print b)

  ())
