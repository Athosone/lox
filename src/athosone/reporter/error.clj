(ns athosone.reporter.error)

(def had-error (atom false))

(defn- report [line where msg]
  (reset! had-error true)
  (println (str "[line " line "] Error" where ": " msg)))

(defn error
  ([line msg]
   (error line "" msg))
  ([line where msg]
   (report line where msg)))

(defn reset-error []
  (reset! had-error false))

