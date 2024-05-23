#!/usr/bin/env bb
(require '[babashka.cli :as cli]
         '[clojure.java.io :as io])

(defn trim-right [s c]
  (when (empty? s) s)
  (if (= (last s)  c)
    (subs s 0 (dec (count s)))
    s))

(def cli-options {:output-dir {:default "~/dev/lox/src/athosone"}
                  :help {:coerce :boolean}})

(cli/parse-opts *command-line-args* {:spec cli-options})

(def expr {:binary {:lhs {:type :expression}
                    :operator {:type :token}
                    :rhs {:type :expression}}
           :grouping {:expression {:type :expression}}
           :literal {:value {:type :object}}
           :unary {:operator {:type :token}
                   :right {:type :expression}}})

; (defn define-ast [path]
;   (with-open [w (clojure.java.io/writer path)]
;     (binding [*out* w]
;       (println "(ns athosone.ast)")
;       (println "(defrecord Binary [lhs operator rhs])")
;       (println "(defrecord Grouping [expression])")
;       (println "(defrecord Literal [value])")
;       (println "(defrecord Unary [operator right])")
;       (println "(defprotocol Visitor")
;       (doseq [[key value] expr]
;         (println (str "  (defn visit-" (name key) " [this " (name key) "])"))
;         (println (str "  (defn accept-" (name key) " [this visitor]"))
;         (println (str "    (visit-" (name key) " visitor " (name key) "))"))
;       (println "(defn accept [this visitor]")
;       (println "  (cond")
;       (doseq [[key value] expr]
;         (println (str "    (instance? " (name key) " this) (accept-" (name key) " this visitor)"))
;         (println (str "    :else (throw (Exception. \"Unknown expression type\")))"))
;       (println "(defn main []")
;       (println "  (println \"Hello, World!\")")
;       (println "  (println (accept (Binary. nil nil nil) nil))")
;       (println ")")))

(defprotocol writer
  (write [this s]))

(defn define-ast [w]
  (.write w "(ns athosone.ast)")
  (.write w "(defrecord Binary [lhs operator rhs])")
  (.write w "(defrecord Grouping [expression])")
  (.write w "(defrecord Literal [value])")
  (.write w "(defrecord Unary [operator right])")
  (.write w "(defprotocol Visitor)")

(defn generate-ast []
  (let [[output-dir] *command-line-args*
        path (str (trim-right  output-dir \/) "/ast.clj")]
    (println path)
    (define-ast path)))

(comment
  (spit "/tmp/ast.clj" "")
  (with-open [w (clojure.java.io/writer  "/tmp/ast.clj" :append false)]
    (define-ast w)
    (println (slurp "/tmp/ast.clj")))
  (slurp "/tmp/ast.clj")
  ;; Create stdout writer
  (binding [*out* (io/writer *out*)]
    (define-ast *out*))

  ())
