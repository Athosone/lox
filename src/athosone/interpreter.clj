(ns athosone.interpreter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as str]))

(declare is-truthy?)

(defmulti interpret :type)

(defmethod interpret ::ast/literal [{::ast/keys [value]}]
  value)

(defmethod interpret ::ast/grouping [{::ast/keys [expression]}]
  (interpret expression))

(defmethod interpret ::ast/unary [{::ast/keys [operator right]}]
  (let [rhs (interpret right)]
    (condp = (::token/type operator)
      ::token/minus (- rhs)
      ::token/bang (not (is-truthy? rhs))
      rhs)))

(defn- plus-operator [left right]
  (if (and (instance? Double left) (instance? Double right))
    (+ left right)
    (str left right)))

(defmethod interpret ::ast/binary [{::ast/keys [lhs operator rhs]}]
  (let [left (interpret lhs)
        right (interpret rhs)
        t (::token/type operator)]
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
  (interpret (parse (scan-tokens (new-scanner "---123.4"))))
  (interpret (parse (scan-tokens (new-scanner "\"z\"+\"abc\""))))
  (interpret (parse (scan-tokens (new-scanner "!(false)"))))

  ())
