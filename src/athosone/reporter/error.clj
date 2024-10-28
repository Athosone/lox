(ns athosone.reporter.error
  (:require [athosone.scanner.token :as token]))

(def had-error (atom false))
(def had-runtime-error (atom false))

(defn- report [line where msg]
  (reset! had-error true)
  (println (str "[line " line "] Error at '" where "': " msg)))

(defn error
  ([line msg]
   (error line "" msg))
  ([line where msg]
   (report line where msg)))

(defn error-at-token [token msg]
  (if (= ::token/eof (::token/type token))
    (error (::token/line token) "end" msg)
    (error (::token/line token) (::token/lexeme token) msg)))

(defn reset-error []
  (reset! had-error false))

(defn runtime-error! [{:keys [msg token] :as error}]
  (reset! had-runtime-error true)
  (let [line (::token/line token)]
    (printf "%s\n[line %d]" msg, line)))
