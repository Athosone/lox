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
(declare binary-fn)

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
  (let [token (current parser)
        token-type (::token/type token)
        value (::token/literal token)
        advanced (advance parser)]
    (cond
      (= token-type ::token/false) (append-expr advanced (ast/literal false))
      (= token-type ::token/true) (append-expr advanced (ast/literal true))
      (= token-type ::token/nil) (append-expr advanced (ast/literal nil))
      (or (= token-type ::token/string) (= token-type ::token/number)) (append-expr advanced (ast/literal value))
      (= token-type ::token/left-paren) (primary-grouping advanced)
      :else (throw (ex-info (format "%s: expect epression instead" (current parser))
                            {:causes #{:lexeme (current parser)}})))))

(defn unary [parser]
  (let [token (current parser)
        token-type (::token/type token)
        advanced (advance parser)]
    (if (or (= token-type ::token/minus)
            (= token-type ::token/banq))
      (let [right (unary advanced)
            expr (:expr right)]
        (append-expr right (ast/unary token expr)))
      (primary parser))))

(defn factor [parser]
  (binary-fn parser #{::token/star ::token/slash} unary))

  ; (loop [p (unary parser)]
  ;   (let [token (current p)
  ;         token-type (::token/type token)
  ;         advanced (advance p)
  ;         lhs (:expr p)]
  ;     (if (or (= token-type ::token/star)
  ;             (= token-type ::token/slash))
  ;       (recur (append-expr (unary advanced) (ast/binary lhs token (:expr (unary advanced)))))
  ;       p))))
  ;
(defn term [parser]
  (binary-fn parser #{::token/plus ::token/minus} factor))

  ; (loop [p (factor parser)]
  ;   (let [token (current p)
  ;         token-type (::token/type token)
  ;         advanced (advance p)
  ;         lhs (:expr p)]
  ;     (if (or (= token-type ::token/minus)
  ;             (= token-type ::token/plus))
  ;       (recur (append-expr (factor advanced) (ast/binary lhs token (:expr (factor advanced)))))
  ;       p))))

(defn comparison [parser]
  (binary-fn parser #{::token/greater-equal ::token/greater ::token/less-equal ::token/less} term))

  ; (loop [p (term parser)]
  ;   (let [token (current p)
  ;         token-type (::token/type token)
  ;         advanced (advance p)
  ;         lhs (:expr p)]
  ;     (if (or (= token-type ::token/greater-equal)
  ;             (= token-type ::token/greater)
  ;             (= token-type ::token/less-equal)
  ;             (= token-type ::token/less))
  ;       (recur (append-expr (term advanced) (ast/binary lhs token (:expr (term advanced)))))
  ;       p))))

(defn- binary-fn [parser token-set downwardfn]
  (loop [p (downwardfn parser)]
    (let [token (current p)
          token-type (::token/type token)
          advanced (advance p)
          lhs (:expr p)]
      (if (token-set token-type)
        (let [right (downwardfn advanced)
              right-expr (:expr right)]
          (recur (append-expr right (ast/binary lhs token right-expr))))
        p))))

(defn equality [parser]
  (binary-fn parser #{::token/equal-equal ::token/bang-equal} comparison))
  ; (loop [p (comparison parser)]
  ;   (let [token (current p)
  ;         token-type (::token/type token)
  ;         advanced (advance p)
  ;         lhs (:expr p)]
  ;     (if (or (= token-type ::token/equal-equal) (= token-type ::token/bang-equal))
  ;       (let [right (comparison advanced)
  ;             right-expr (:expr right)]
  ;         (recur (append-expr right (ast/binary lhs token right-expr))))
  ;       p))))
  ;
(defn expression [parser] (equality parser))

(comment
  (require
   '[athosone.prettyprinter :refer [pretty-print]]
   '[athosone.scanner.scan :refer [scan-tokens new-scanner]])
  (if (#{"toto" "tata"} "toto")
    (println "yes")
    (println "no"))
  (def source "1 == 1")
  (def tokens (scan-tokens (new-scanner source)))
  (def p (new-parser tokens))

  (pretty-print (:expr (expression (new-parser (scan-tokens (new-scanner "1 * -1 - -2 == 3"))))))
  ; "(== (- (* 1.0 (group (+ (- 1.0) (- 1.0)))) (- 2.0)) 3.0)"
  ; "(== (- (* 1.0 (group (+ (- 1.0) (- 1.0)))) (- 2.0)) 3.0)"
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
