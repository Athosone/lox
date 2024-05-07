(ns athosone.lox
  (:require
   [athosone.reporter.error :refer [error reset-error had-error]]
   [athosone.scanner.scan :refer [new-scanner]]))

(defn print-fl [msg]
  (print msg)
  (flush))

(defn run [source]
  (let [scanner (new-scanner source)
        tokens (scanner scan)]
    (for [token tokens]
      (println token))))

(defn run-file [path]
  (let [source (slurp path)]
    (run source)
    (if @had-error
      (System/exit 65)
      (System/exit 0))))

(defn run-prompt []
  (loop []
    (print-fl "> ")
    (let [line (read-line)]
      (if line
        (do
          (run line)
          (reset-error)
          (recur))
        (println "Bye!")))))

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
  (use 'athosone.lox :reload)
  (run "print 1 + 2;")
  (error 1 "Error message")
  @had-error

  (run-file "Makefile")

  (-main))

