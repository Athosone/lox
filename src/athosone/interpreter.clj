(ns athosone.interpreter
  (:require
   [athosone.ast :as ast]
   [athosone.scanner.token :as token]
   [clojure.string :as str]
   [athosone.reporter.error :as error]))

(declare is-truthy?)
(declare plus-operator)
(declare assert-number)
(declare assert-numbers)

(defmulti evaluate :type)

(defmethod evaluate ::ast/literal [{::ast/keys [value]}]
  value)

(defmethod evaluate ::ast/grouping [{::ast/keys [expression]}]
  (evaluate expression))

(defmethod evaluate ::ast/unary [{::ast/keys [operator right]}]
  (let [rhs (evaluate right)
        partial-assert (partial assert-numbers operator)]
    (condp = (::token/type operator)
      ::token/minus (last ((juxt partial-assert -) rhs))
      ::token/bang (not (is-truthy? rhs))
      rhs)))

(defmethod evaluate ::ast/binary [{::ast/keys [lhs operator rhs]}]
  (let [left (evaluate lhs)
        right (evaluate rhs)
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

(defn interpret [expr]
  (try
    (let [value (evaluate expr)]
      (println (str value))
      value)
    (catch Exception e
      (case (:type (ex-data e))
        :runtime-error (athosone.reporter.error/runtime-error (ex-data e))
        (throw e)))))

(defn- assert-numbers [operator & n]
  (when-not (every? number? n)
    (throw (ex-info "Operands must be a number"
                    {:type :runtime-error
                     :msg "Operands must be a number"
                     :token operator
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
  (assert-number "1")

  (#{1, 2} 4)
  (interpret (parse (scan-tokens (new-scanner "---\"a\""))))
  (interpret (parse (scan-tokens (new-scanner "---123.4"))))
  (interpret (parse (scan-tokens (new-scanner "1<\"1\""))))
  (interpret (parse (scan-tokens (new-scanner "1<1"))))
  (interpret (parse (scan-tokens (new-scanner "\"1\"==\"1\""))))
  (interpret (parse (scan-tokens (new-scanner "\"z\"+\"abc\""))))
  (interpret (parse (scan-tokens (new-scanner "!(false)"))))

  ())
