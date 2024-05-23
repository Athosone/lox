(ns athosone.prettyprinter
  (:require [athosone.ast :as ast]))

(defmulti pretty-print :type)

(defmethod pretty-print ::ast/unary [{::ast/keys [operator right left]}]
  (println (str operator " " right left)))

(comment
  (def a {:type ::ast/unary ::ast/operator "hello" ::ast/right "world"})
  (def b {:type ::ast/grouping ::ast/operator "hello" ::ast/right "world" ::ast/left "www"})
  (pretty-print a)
  (pretty-print b)

  ())
