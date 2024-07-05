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

  (interpret (parse (scan-tokens (new-scanner "---123.4"))))
  (interpret (parse (scan-tokens (new-scanner "!(false)"))))

  ())
