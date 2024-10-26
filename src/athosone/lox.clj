(ns athosone.lox
  (:require
   [athosone.interpreter :refer [interpret]]
   [athosone.parser :as parser]
   [athosone.prettyprinter :refer [pretty-print]]
   [athosone.reporter.error :refer [error had-error had-runtime-error
                                    reset-error]]
   [athosone.scanner.scan :refer [new-scanner scan-tokens]]))

(defn print-fl [msg]
  (print msg)
  (flush))

(defn run [source]
  (let [scanner (new-scanner source)
        tokens (scan-tokens scanner)
        p (parser/parse tokens)]
    (if (= nil p)
      (println p)
      (do (println (pretty-print p))
          (interpret p)))))

(defn run-file [path]
  (let [source (slurp path)]
    (run source)
    (if @had-runtime-error
      (System/exit 70)
      (System/exit 0))
    (if @had-error
      (System/exit 65)
      (System/exit 0))))

(defn run-prompt []
  (print-fl "> ")
  (if-let [line (read-line)]
    (do
      (run line)
      (reset-error)
      (recur))
    (println "Goodbye!")))

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
  (run "1 + 2")
  (error 1 "Error message")
  @had-error
  (def abc ["a", "b", "c"])
  (doseq [a abc]
    (println a))
  (run-file "Makefile")
  (run-prompt)

  (-main))

