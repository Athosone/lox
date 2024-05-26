(ns athosone.ast
  (:require
   [athosone.prettyprinter :refer [PrettyPrintable]]))
;;{:binary {:lhs {:type :expression}, :operator {:type :token}, :rhs {:type :expression}}, :grouping {:expression {:type :expression}}, :literal {:value {:type :object}}, :unary {:operator {:type :token}, :right {:type :expression}}}
  ;; "Binary   : Expr left, Token operator, Expr right",
  ;;     "Grouping : Expr expression",
  ;;     "Literal  : Object value",
  ;;     "Unary    : Token operator, Expr right"

(defn unary [operator right]
  {::type ::unary ::operator operator ::right right})

(defn binary [lhs operator rhs]
  {::type ::binary ::lhs lhs ::operator operator ::rhs rhs})

(defn grouping [expr]
  {::type ::grouping ::expression expr})

(defn literal [value]
  {::type ::literal ::value value})

;; -- Todo experiment with records
(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Unary [operator right])
(defrecord Binary [lhs operator rhs])
(defrecord Expression [value])

(extend-type Literal
  PrettyPrintable
  (pprint [this]
    (println (str "(" (:value this) ")"))))

;; -- End Todo

(comment
  (def l (Literal. "world"))
  (:value l)
  ())
