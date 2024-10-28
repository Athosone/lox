(ns athosone.parser
  (:require
   [athosone.ast :as ast]
   [athosone.prettyprinter :refer [pretty-print]]
   [athosone.reporter.error :refer [error-at-token]]
   [athosone.scanner.token :as token]))

(declare comma)
(declare expression)
(declare binary-fn)

(defn new-parser [tokens]
  {:tokens tokens
   :statements []
   :current 0
   :expr nil})

(defn- current [{:keys [current tokens]}]
  (tokens current))

(defn- token-type [parser] (::token/type (current parser)))

(defn- at-end? [parser] (= (::token/type (current parser)) ::token/eof))

(defn- append-expr [parser expr] (assoc-in parser [:expr] expr))

(defn- advance [p]
  (if (not (= (token-type p) ::token/eof))
    (update p :current inc)
    p))

(defn synchronize [parser]
  (let [advanced (advance parser)
        current-token-type (token-type parser)
        next-token-type (token-type advanced)]
    (if (or (= next-token-type ::token/eof) (= current-token-type ::token/semicolon))
      advanced
      (condp = next-token-type
        ::token/class advanced
        ::token/fun advanced
        ::token/var advanced
        ::token/for advanced
        ::token/if advanced
        ::token/while advanced
        ::token/print advanced
        ::token/return advanced
        (recur advanced)))))

(defn- consume [parser expected-token-type]
  (if-not (= expected-token-type (token-type parser))
    (let [token (current parser)
          msg (format "Expected %s token instead of %s"
                      expected-token-type (token-type parser))]
      (error-at-token token msg)
      (throw (ex-info msg {:expected-token-type expected-token-type})))
    (advance parser)))

(defn- primary-grouping [parser]
  (let [exp (comma parser)]
    (append-expr (consume exp ::token/right-paren) (ast/grouping (:expr exp)))))

(defn- primary [parser]
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
            (= token-type ::token/bang))
      (let [right (unary advanced)
            expr (:expr right)]
        (append-expr right (ast/unary token expr)))
      (primary parser))))

(defn factor [parser]
  (binary-fn parser #{::token/star ::token/slash} unary))

(defn term [parser]
  (binary-fn parser #{::token/plus ::token/minus} factor))

(defn comparison [parser]
  (binary-fn parser #{::token/greater-equal ::token/greater ::token/less-equal ::token/less} term))

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

(defn expression [parser] (equality parser))

(defn comma [parser]
  (binary-fn parser #{::token/comma} expression))

(defn- match? [parser token-type])

(defn- print-statement [parser])

(defn statement [parser]
  (cond
    (match? parser ::token/print) (print-statement parser)
    :else (comma parser)))

(defn parse [tokens]
  (loop [parser (new-parser tokens)]
    (if (at-end? parser)
      [(:statements parser)]
      (recur (statement parser)))))

;; (defn parse [tokens]
;;   (let [parser (new-parser tokens)]
;;     (try
;;       (:expr (comma parser))
;;       (catch Exception e
;;         (do
;;           (println e)
;;           nil)))))

; Precedence level in inversely propoertionate to the order of the code
;; expression     → equality ;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary
;;                | primary ;
;; primary        → NUMBER | STRING | "true" | "false" | "nil"
;;                | "(" expression ")" ;
;; Here factor has a higher precedence over term
;;
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

  (pretty-print (parse tokens))
  (synchronize p)
  (pretty-print (:expr (expression (new-parser (scan-tokens (new-scanner "!true"))))))
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
  (parse (scan-tokens (new-scanner "a + b")))
  (:expr (unary (new-parser (scan-tokens (new-scanner "-1 * 1")))))
  (:expr (factor (new-parser (scan-tokens (new-scanner "-1 * -1 / 0")))))
  (factor (new-parser (scan-tokens (new-scanner "-1 * 1 / 0"))))
  (:expr (equality p))
  ())
