(ns athosone.lox
  (:require [athosone.scanner.scan :refer [new-scanner scan]]))

(defn print-fl [msg]
  (print msg)
  (flush))

(defn run-file [path] (slurp path))
(defn run-prompt []
  (loop []
    (print-fl "> ")
    (let [line (read-line)]
      (if line
        (do
          (println line)
          (recur))
        (println "Bye!")))))

(defn run [source]
  (let [scanner (new-scanner source)
        tokens (scan)]
    (for [token tokens]
      (println token))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (cond
    (> (count args) 1) (println "Usage: lox <script>")
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))
;; To Continue:
;; https://craftinginterpreters.com/scanning.html#error-handling
(comment
  (run-prompt)
  (run "print 1 + 2;")

  (run-file "Makefile")

  (-main))

