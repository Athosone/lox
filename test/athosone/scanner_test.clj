(ns athosone.scanner-test
  (:require [clojure.test :refer :all]
            [athosone.scanner.scan :as scan]
            [athosone.scanner.token :as token]))

(deftest invalid-character
  (testing "given an invalid character when scanning token should get an error"
    (let [test-scanner (scan/new-scanner "@")
          scanner (scan/scan-token test-scanner)]
      (is (= 1 (count (:errors scanner)))))))

(deftest equal-as-second-character
  (testing "given an equal as second character when scanning token should get an equal token")
  (let [test-scanner (scan/new-scanner "!=")
        tokens (scan/scan-tokens test-scanner)]
    (is (= 2 (count tokens)))
    (is (= ::token/bang-equal (::token/type (first tokens))))))

(deftest equal-as-first-character
  (testing "given an equal as first character when scanning token should get an equal token")
  (let [test-scanner (scan/new-scanner "=")
        tokens (scan/scan-tokens test-scanner)]
    (is (= 2 (count tokens)))
    (is (= ::token/equal (::token/type (first tokens))))))

(deftest slash-as-character
  (testing "given a slash as character when scanning token should get a slash token")
  (let [test-scanner (scan/new-scanner "/")
        tokens (scan/scan-tokens test-scanner)]
    (is (= 2 (count tokens)))
    (is (= ::token/slash (::token/type (first tokens))))))

(comment
  (require '[clojure.spec.alpha :refer [explain]])
  (def scanner (scan/scan-tokens (scan/new-scanner "!=")))
  (def token (first (scan/scan-tokens scanner)))
  (::token/type token)
  (map #(println %) token))
