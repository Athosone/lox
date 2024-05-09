(ns athosone.scanner.scan
  (:require [athosone.scanner.token :as token]))

(defn new-scanner [source]
  {:source source
   :start 0
   :current 0
   :line 1
   :tokens []})

(defn is-at-end? [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn add-token
  ([type scanner] (add-token type nil scanner))
  ([type literal {:keys [source current start line] :as scanner}]
   (update scanner :tokens conj  {::token/type type
                                  ::token/text (subs source start current)
                                  ::token/line line
                                  ::token/literal literal})))

(defn scan-token [scanner]
  (println "Scanning token")
  (println scanner)
  (let [c (nth (:source scanner) (:current scanner))
        scanner (update scanner :current inc)]
    (cond
      (= c \() (add-token ::token/left-paren scanner)
      (= c \)) (add-token ::token/right-paren scanner)
      (= c \{) (add-token ::token/left-brace scanner)
      (= c \}) (add-token ::token/right-brace scanner)
      (= c \,) (add-token ::token/comma scanner)
      (= c \.) (add-token ::token/dot scanner)
      (= c \-) (add-token ::token/minus scanner)
      (= c \+) (add-token ::token/plus scanner)
      (= c \*) (add-token ::token/star scanner)
      :else (add-token ::token/eof scanner))))

(defn inc-current [scanner] (update scanner :start inc))

(defn scan-tokens
    ;; Returns a vector of tokens
  ([sc]
   (loop [scanner sc]
     (if (is-at-end? scanner)
       (conj (:tokens scanner) (add-token ::token/eof scanner))
       (recur (-> scanner
                  (scan-token)
                  (inc-current)))))))

(comment
  (def scanner (new-scanner "(+ 123)"))
  (is-at-end? (assoc scanner :current 8000))
  (nth (:source scanner) 0)
  (scan-tokens (assoc scanner :current 0))
  (assoc scanner :current 0)
  (def news (-> scanner
                (assoc :tokens (conj  (:tokens scanner) (scan-token scanner)))
                (update scanner :current inc)))
  (-> scanner
      (scan-token)
      (inc-current))
  (println news))


