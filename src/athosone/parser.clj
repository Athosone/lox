(ns athosone.parser
  (:require
   [athosone.ast :as ast]
   [athosone.prettyprinter :refer [pretty-print]]
   [athosone.scanner.token :as token]))

(defn new-parser [tokens]
  {:tokens tokens :current 0 :expr nil})

(defn- current [{:keys [current tokens]}]
  (tokens current))

(defn- token-type [parser] (::token/type (current parser)))
(defn- token-line [parser] (::token/line (current parser)))

(declare expression)

(defn append-expr [parser expr] (assoc-in parser [:expr] expr))

(defn- advance [p]
  (if (not (= (token-type p) ::token/eof))
    (update p :current inc)
    p))

(defn- primary-grouping [parser]
  (let [exp (expression parser)]
    (if (= (::token/type (current exp)) ::token/right-paren)
      (append-expr (advance exp) (ast/grouping (:expr exp)))
      (throw (ex-info (format "Expect right-paren after expression: %s at line: %d" (token-type exp) (token-line exp))
                      {:causes #{:missing-expr "Should have a right paren"}})))))

(defn primary [parser]
  (let [operator (current parser)
        operator-type (::token/type operator)
        value (::token/literal operator)
        advanced (advance parser)]
    (cond
      (= operator-type ::token/false) (append-expr advanced (ast/literal false))
      (= operator-type ::token/true) (append-expr advanced (ast/literal true))
      (= operator-type ::token/nil) (append-expr advanced (ast/literal nil))
      (or (= operator-type ::token/string) (= operator-type ::token/number)) (append-expr advanced (ast/literal value))
      (= operator-type ::token/left-paren) (primary-grouping advanced)
      :else (throw (ex-info (format "%s: expect epression instead" (current parser))
                            {:causes #{:lexeme (current parser)}})))))

(defn unary [parser]
  (let [operator (current parser)
        operator-type (::token/type operator)
        advanced (advance parser)]
    (if (or (= operator-type ::token/minus)
            (= operator-type ::token/banq))
      (let [right (unary advanced)
            expr (:expr right)]
        (append-expr right (ast/unary operator expr)))
      (primary parser))))

(defn factor [parser]
  (loop [p (unary parser)]
    (let [operator (current p)
          operator-type (::token/type operator)
          advanced (advance p)
          lhs (:expr p)]
      (if (or (= operator-type ::token/star)
              (= operator-type ::token/slash))
        (recur (append-expr (unary advanced) (ast/binary lhs operator (:expr (unary advanced)))))
        p))))

(defn term [parser]
  (loop [p (factor parser)]
    (let [operator (current p)
          operator-type (::token/type operator)
          advanced (advance p)
          lhs (:expr p)]
      (if (or (= operator-type ::token/minus)
              (= operator-type ::token/plus))
        (recur (append-expr (factor advanced) (ast/binary lhs operator (:expr (factor advanced)))))
        p))))

(defn comparison [parser]
  (loop [p (term parser)]
    (let [operator (current p)
          operator-type (::token/type operator)
          advanced (advance p)
          lhs (:expr p)]
      (if (or (= operator-type ::token/greater-equal)
              (= operator-type ::token/greater)
              (= operator-type ::token/less-equal)
              (= operator-type ::token/less))
        (recur (append-expr (term advanced) (ast/binary lhs operator (:expr (term advanced)))))
        p))))

;; Trying to refactor binary expressions
;; (defn- new-function [parser token-set downwardfn]
;;   (loop [p (downwardfn parser)]
;;     (let [operator (current p)
;;           operator-type (::token/type operator)
;;           advanced (advance p)
;;           lhs (:expr p)]
;;       (if (token-set operator-type)
;;         (let [right (downwardfn advanced)
;;               right-expr (:expr right)]
;;           (recur (append-expr right (ast/binary lhs operator right-expr))))
;;         p))))

(defn equality [parser]
  (loop [p (comparison parser)]
    (let [operator (current p)
          operator-type (::token/type operator)
          advanced (advance p)
          lhs (:expr p)]
      (if (or (= operator-type ::token/equal-equal) (= operator-type ::token/bang-equal))
        (let [right (comparison advanced)
              right-expr (:expr right)]
          (recur (append-expr right (ast/binary lhs operator right-expr))))
        p))))

(defn expression [parser] (equality parser))

(comment
  (require
   '[athosone.prettyprinter :refer [pretty-print]]
   '[athosone.scanner.scan :refer [scan-tokens new-scanner]])
  (def source "1 == 1")
  (def tokens (scan-tokens (new-scanner source)))
  (def p (new-parser tokens))

  (pretty-print (:expr (expression (new-parser (scan-tokens (new-scanner "1 * -1 - -2 == 3"))))))
  (pretty-print (:expr (expression (new-parser (scan-tokens (new-scanner "1 * (-1 + -1) - -2 == 3"))))))
  (pretty-print (:expr (expression (new-parser (scan-tokens (new-scanner "-9 - (1 + 1 * (1 / 2)"))))))
  (prn p)
  (assoc-in p [:expr] "hellp")

  (:current (expression p))
  (:expr (unary p))
  (primary (new-parser (scan-tokens (new-scanner "1 + 1"))))
  (:expr (unary (new-parser (scan-tokens (new-scanner "-1 * 1")))))
  (:expr (factor (new-parser (scan-tokens (new-scanner "-1 * -1 / 0")))))
  (factor (new-parser (scan-tokens (new-scanner "-1 * 1 / 0"))))
  (:expr (equality p))
  ())
