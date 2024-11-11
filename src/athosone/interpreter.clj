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

(defmulti execute (fn [x] (first (keys x))))

(defmethod execute :stmt/print [stmt]
  (let [value (evaluate (:stmt/print stmt))]
    (println (str value))))

(defmethod execute :stmt/expr [stmt]
  (evaluate (:stmt/expr stmt)))

(defmethod execute :default [stmt]
  (println "Unknown statement: " stmt))

(defn interpret [stmts]
  (try
    (for [stmt stmts]
      (execute stmt))
    (catch Exception e
      (case (:type (ex-data e))
        :runtime-error (athosone.reporter.error/runtime-error! (ex-data e))
        (throw e)))))

;; (defn interpret [expr]
;;   (try
;;     (let [value (evaluate expr)]
;;       (println (str value))
;;       value)
;;     (catch Exception e
;;       (case (:type (ex-data e))
;;         :runtime-error (athosone.reporter.error/runtime-error! (ex-data e))
;;         (throw e)))))

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

  (-> "1 == 1;
      print 1;"
      new-scanner
      scan-tokens
      parse
      first
      interpret)
  ((comp (partial assert-number "toto") -) 1)

  ())
