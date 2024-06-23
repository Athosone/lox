(ns athosone.parser-test
  (:require
   [athosone.ast :as ast]
   [athosone.parser :as parser]
   [athosone.scanner.scan :refer [new-scanner scan-tokens]]
   [athosone.scanner.token :as token]
   [clojure.test :refer :all]))

(defn- parser-from-str [s]
  (-> s
      new-scanner
      scan-tokens
      parser/new-parser))

(defn- token-type [token] (:type token))

(deftest negative-unary
  (testing "Given a negative number when parsing then create a unary element"
    (let [p (parser-from-str "-1")
          parsed (parser/expression p)
          expr (:expr parsed)
          t (::ast/operator expr)]
      (is (= (:type expr) ::ast/unary))
      (is (= (::token/type t) ::token/minus))
      (is (nil? (::token/literal t))))))

(deftest primary
  (testing "Given a primary when parsing then create a literal element"
    (let [p (parser-from-str "1")
          parsed (parser/expression p)
          expr (:expr parsed)
          t (::ast/operator expr)]
      (is (= (:type expr) ::ast/literal))
      (is (= (::ast/value expr)  1.0)))))

