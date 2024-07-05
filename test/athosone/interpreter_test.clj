(ns athosone.interpreter-test
  (:require
   [athosone.interpreter :refer :all]
   [athosone.parser :as parser]
   [athosone.scanner.scan :refer [new-scanner scan-tokens]]
   [clojure.test :refer :all]))

(deftest interpret-test
  (testing "Given a parsed unary string when interpret the correct output is returned"
    (is (= -123.4 (interpret (parser/parse (scan-tokens (new-scanner "-123.4"))))))
    (is (= -123.4 (interpret (parser/parse (scan-tokens (new-scanner "---123.4"))))))
    (is (= false (interpret (parser/parse (scan-tokens (new-scanner "!(!false)"))))))
    (is (= true (interpret (parser/parse (scan-tokens (new-scanner "!(false)"))))))
    (is (= 123.4 (interpret (parser/parse (scan-tokens (new-scanner "--123.4"))))))))
