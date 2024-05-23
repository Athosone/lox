(ns athosone.ast)
;;{:binary {:lhs {:type :expression}, :operator {:type :token}, :rhs {:type :expression}}, :grouping {:expression {:type :expression}}, :literal {:value {:type :object}}, :unary {:operator {:type :token}, :right {:type :expression}}}

(defn unary [operator right]
  {::type :unary ::operator operator ::right right})

(defn binary [lhs operator rhs]
  {::type :binary ::lhs lhs ::operator operator ::rhs rhs})


(defrecord Grouping [expression])
(defrecord Literal [value])
(defrecord Unary [operator right])
(defrecord Binary [lhs operator rhs])

(comment 
  (def a (Grouping. (Literal. 1)))
