(ns athosone.parser
  (:require [athosone.scanner.token :as token]
            [athosone.ast :as ast]))

(defn new-parser [tokens]
  {:tokens tokens :current 0 :expr nil})

(defn- current [{:keys [current tokens]}]
  (tokens current))

(defn- previous [parser] (current (update parser :current dec)))

(defn- match [parser token]
  (= (current parser) token))

(defn- is-at-end? [parser] (= (current parser) ::token/eof))

(defn append-expr [parser expr] (assoc-in parser [:expr] expr))

(defn- advance [p]
  (if (not (= (current p) ::token/eof))
    (update p :current inc)
    p))

(defn- primary-grouping [parser]
  (let [exp (expression parser)]
    (append-expr parser exp)))

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
      (= operator-type ::token/left-paren) (primary-grouping advanced))))

(defn unary [parser]
  (let [operator (current parser)
        operator-type (::token/type operator)
        advanced (advance parser)]
    (if (or (= operator-type ::token/minus)
            (= operator-type ::token/banq))
      (let [right (unary advanced)]
        (ast/unary operator right))
      (primary parser))))

(defn factor [parser]
  (loop [p (unary parser)
         e (:expr p)]
    (let [operator (current p)
          operator-type (::token/type operator)]
      (if (or (= operator-type ::token/star)
              (= operator-type ::token/slash))
        (recur (advance p)
               (append-expr p (ast/binary e operator (unary (advance p)))))
        (append-expr p e)))))

(defn term [parser]
  (loop [p (factor parser)
         e (:expr p)]
    (let [operator (current p)
          operator-type (::token/type operator)]
      (if (or (= operator-type ::token/minus)
              (= operator-type ::token/plus))
        (recur (advance p)
               (append-expr p (ast/binary e operator (factor (advance p)))))
        (append-expr p e)))))

(defn comparison [parser]
  (loop [p (term parser)
         e (:expr p)]
    (let [operator (current p)
          operator-type (::token/type operator)]
      (if (or (= operator-type ::token/greater-equal)
              (= operator-type ::token/greater)
              (= operator-type ::token/less-equal)
              (= operator-type ::token/less))
        (recur (advance p)
               (append-expr p (ast/binary e operator (term (advance p)))))
        (append-expr p e)))))

(defn equality [parser]
  (loop [p (comparison parser)
         e (:expr p)]
    (let [operator (current p)
          operator-type (::token/type operator)]
      (println "operator-type" operator-type)
      (if (or (= operator-type ::token/equal-equal) (= operator-type ::token/bang-equal))
        (recur (advance p) (append-expr p (ast/binary e operator (comparison (advance p)))))
        (append-expr p e)))))

(defn expression [parser] (equality parser))

(comment
  (require '[athosone.scanner.scan :refer [scan-tokens new-scanner]])
  (def source "-1 == 1")
  (def tokens (scan-tokens (new-scanner source)))
  (def p (new-parser tokens))

  (prn p)
  (assoc-in p [:expr] "hellp")

  (:current (expression p))
  (:expr (unary p))
  (primary p)
  (:expr (equality p))
  ())
