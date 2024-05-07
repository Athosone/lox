(ns athosone.scanner.scan
  (:require [athosone.scanner.token :as token]))

(defn new-scanner [source]
  {:source source
   :start 0
   :current 0
   :line 1
   :tokens []})

(defn scan-tokens
  ;; Initiate scan tokens function
  ([source] (scan-tokens source 0 0 1 []))
  ;; Returns a vector of tokens
  ([source
    start
    current
    line
    tokens]
   (if (>= current (count source))
     (conj tokens ::token/eof)
     (let [c (get source current)]
       (cond
         (= c \() (recur source (inc start) (inc current) line (conj tokens ::token/left-paren))
         (= c \)) (recur source (inc start) (inc current) line (conj tokens ::token/right-paren))
         (= c \{) (recur source (inc start) (inc current) line (conj tokens ::token/left-brace))
         (= c \}) (recur source (inc start) (inc current) line (conj tokens ::token/right-brace))
         (= c \,) (recur source (inc start) (inc current) line (conj tokens ::token/comma))
         (= c \.) (recur source (inc start) (inc current) line (conj tokens ::token/dot))
         (= c \-) (recur source (inc start) (inc current) line (conj tokens ::token/minus))
         (= c \+) (recur source (inc start) (inc current) line (conj tokens ::token/plus))
         (= c \*) (recur source (inc start) (inc current) line (conj tokens ::token/star)))))))

(comment
  (scan-tokens "hello")
  (get "hello" 0)
  (nth "hello" 0)
  (h hash))


