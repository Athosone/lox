(ns athosone.interpreter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as str]))

(declare is-truthy?)
(declare plus-operator)
(declare assert-number)
(declare assert-numbers)

(defmulti interpret :type)

(defmethod interpret ::ast/literal [{::ast/keys [value]}]
  value)

(defmethod interpret ::ast/grouping [{::ast/keys [expression]}]
  (interpret expression))

(defmethod interpret ::ast/unary [{::ast/keys [operator right]}]
  (let [rhs (interpret right)
        partial-assert (partial assert-numbers operator)]
    (condp = (::token/type operator)
      ::token/minus (last ((juxt partial-assert -) rhs))
      ::token/bang (not (is-truthy? rhs))
      rhs)))

(defmethod interpret ::ast/binary [{::ast/keys [lhs operator rhs]}]
  (let [left (interpret lhs)
        right (interpret rhs)
        t (::token/type operator)]
    (when-not (#{::token/plus ::token/equal-equal ::token/bang-equal} t)
      ;; (prn operator lhs rhs))
      (assert-numbers operator left right))
    (condp = t
      ::token/greater (> left right)
      ::token/greater-equal (>= left right)
      ::token/less (< left right)
      ::token/less-equal (<= left right)
      ::token/slash (/ left right)
      ::token/minus (- left right)
      ::token/star (* left right)
      ::token/plus (plus-operator left right)
      ::token/equal-equal (= left right)
      ::token/bang-equal (not= left right)
      nil)))

(defn- assert-numbers [operator & n]
  (when-not (every? number? n)
    (throw (ex-info "Operands must be a number"
                    {:cause "Passed numbers aren't numbers"
                     :operator operator
                     :numbers (apply str n)}))))

(defn- plus-operator [left right]
  (if (and (number? left) (number? right))
    (+ left right)
    (str left right)))

(defn- is-truthy? [value]
  (cond
    (nil? value) false
    (instance? Boolean value) value
    :else true))

;; (defmethod interpret ::ast/)

(comment
  (require '[athosone.prettyprinter :refer [pretty-print]]
           '[athosone.parser :as parser :refer [parse]]
           '[athosone.scanner.scan :refer [scan-tokens new-scanner]])

  (str 1 2)
  (def x 1)
  ((comp (partial assert-number "toto") -) 1)

  (#{1, 2} 4)
  (interpret (parse (scan-tokens (new-scanner "---\"a\""))))
  (interpret (parse (scan-tokens (new-scanner "---123.4"))))
  (interpret (parse (scan-tokens (new-scanner "1<\"1\""))))
  (interpret (parse (scan-tokens (new-scanner "1<1"))))
  (interpret (parse (scan-tokens (new-scanner "\"1\"==\"1\""))))
  (interpret (parse (scan-tokens (new-scanner "\"z\"+\"abc\""))))
  (interpret (parse (scan-tokens (new-scanner "!(false)"))))

  ())
