(ns athosone.scanner.scan
  (:require
   [athosone.reporter.error :as error]
   [athosone.scanner.token :as token]))

(defn new-scanner [source]
  {:source source
   :start 0
   :current 0
   :line 1
   :tokens []
   :errors []})

(defn is-at-end? [scanner]
  (>= (:current scanner) (count (:source scanner))))

(defn add-token
  ([type scanner] (add-token type nil scanner))
  ([type literal {:keys [source start current line] :as scanner}]
   (update scanner :tokens conj  {::token/type type
                                  ::token/text (subs source start current)
                                  ::token/line line
                                  ::token/literal literal})))

(defn- current-char [& {:keys [source current]}] (nth source current))

(defn- match [scanner expected]
  (and (not (is-at-end? scanner))
       (= (current-char scanner) expected)))

(defn- advance [scanner]
  (update scanner :current inc))

(defn scan-token [scanner]
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
      (= c \!) (if (match scanner \=)
                 (add-token ::token/bang-equal (advance scanner))
                 (add-token ::token/bang scanner))
      (= c \=) (if (match scanner \=)
                 (add-token ::token/equal-equal (advance scanner))
                 (add-token ::token/equal scanner))
      (= c \<) (if (match scanner \=)
                 (add-token ::token/less-equal (advance scanner))
                 (add-token ::token/less scanner))
      (= c \>) (if (match scanner \=)
                 (add-token ::token/greater-equal (advance scanner))
                 (add-token ::token/greater scanner))
      (= c \/) (if (match scanner \/)
                 (loop [scanner scanner]
                   (if (not (or (match scanner \newline)
                                (is-at-end? scanner)))
                     (recur (advance scanner))
                     scanner))
                 (add-token ::token/slash scanner))
      (= c \space) scanner
      (= c \tab) scanner
      (= c \newline) (advance (update scanner :line inc))
      :else (update scanner :errors conj
                    (error/error (:line scanner) (current-char scanner) "Unexpected character")))))

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
  (def scanner (new-scanner "("))
  (def scanner (new-scanner "==
!="))
  (is-at-end? (assoc scanner :current 8000))
  (nth (:source scanner) 0)
  (scan-tokens (assoc scanner :current 0))
  (assoc scanner :current 0)
  (def news (-> scanner
                (assoc :tokens (conj  (:tokens scanner) (scan-token scanner)))
                (update scanner :current inc)))
  (def expected \=)

  (scan-token scanner)
  (loop [scanner scanner]
    (when (not (or (match scanner \newline) (is-at-end? scanner)))
      (println (current-char scanner) scanner)
      (recur (advance scanner))))

  (-> scanner
      (scan-token)
      (inc-current))

  (println news))


