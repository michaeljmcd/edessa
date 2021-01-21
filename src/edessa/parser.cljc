(ns edessa.parser
 (:require [clojure.core.reducers :refer [fold]]
           [taoensso.timbre :as t :refer [debug error]]))

; General parsing functions and combinators.

(defn make-input [inp]
  {:input inp 
   :position 0 
   :line-number 0 
   :column 0
   :result []
   :failed false
   :error nil})

(defn mask-result [inp]
  (assoc inp :result []))

(defn apply-parser [p inp]
  (if (map? inp)
    (p inp)
    (p (make-input inp))))

(defn result [inp] (get inp :result))

(defn succeed {:parser "Succeed"} [v inp]
  (assoc inp :result (conj (result inp) v)))

(defn succeed! {:parser "Succeed!"} [v inp]
  (assoc inp :result v))

(defn parser-name [parser] (-> parser meta :parser))

(defn discard [p]
  (with-meta
    (fn [inp]
      (let [input-result (get inp :result)]
        (assoc (p inp)
               :result input-result)
      ))
    {:parser (str (parser-name p) ", discarding output")}))

(defn advance 
  ([inp] 
    (let [{left :input
           pos :position
           line :line-number
           col :column
           res :result
           err :error
           fail :failed} inp]
     {:input (rest left)
      :position (inc pos)
      :line-number (if (= \newline (first left))
                    (inc line)
                    line)
      :column (if (= \newline (first left))
               0
               (inc col))
      :result res
      :error err
      :failed fail}))
  ([inp v] (advance (succeed v inp)))
  )

(defn remaining [inp] (get inp :input))

(defn look [inp]
  (first (get inp :input)))

(def epsilon 
  (with-meta (partial succeed nil) 
             {:parser "Epsilon (empty)"}))

(defn fail {:parser "Fail"} 
  ([inp] (fail inp "Parsing failed"))
  ([inp message] 
   (-> inp
       (assoc :error message)
       (assoc :failed true))))

(defn failure? [r] 
  (get r :failed))

(def success? (comp not failure?))

(defn input-consumed? [r]
 (and (success? r)
      (empty? (get r :input))))

(def input-remaining? (comp not input-consumed?))

(defn match [c]
  (with-meta
    (fn [inp]
      (if (and (input-remaining? inp)
               (= (look inp) c))
        (advance inp c)
        (fail inp (format "The value '%s' does not match the expected value of '%s'", (look inp) c))))
    {:parser (str "Matches " c)}))

(defn not-one-of [chars]
  (with-meta
    (fn [inp]
      (let [x (look inp)]
        (if (or (input-consumed? inp)
                (some (partial = x) chars))
          (fail inp (format "Value '%s' is not one of %s" x chars))
          (advance inp x))))
    {:parser (str "Not one of [" chars "]")}))

(defn zero-or-more [parser]
  (letfn [(accumulate [inp]
            (debug "Z*: " (parser-name parser) " Input: " inp)
            (if (input-consumed? inp)
              inp
              (let [r (parser inp)]
                (debug "Z*: Parser " (parser-name parser) " yielded " r)
                (if (failure? r)
                  (do
                    (debug "Z*: Hit end of matches, returning " inp)
                    inp)
                  (recur r)))))]
    (with-meta
      (fn [inp] 
        (let [result (accumulate (assoc inp :result []))]
          (assoc result :result (conj (:result inp) (:result result)))
          )
      )
      {:parser (->> parser parser-name (str "Zero or more "))})))

(def star zero-or-more)

(defn choice
  ([] (with-meta fail {:parser "Fail"}))
  ([parser1] (with-meta parser1 {:parser (parser-name parser1)}))
  ([parser1 parser2]
   (with-meta
     (fn [inp]
       (let [r1 (parser1 inp)]
         (if (failure? r1)
           (parser2 inp)
           r1)))
     {:parser (str (parser-name parser1) " OR " (parser-name parser2))}))
  ([parser1 parser2 & parsers] (fold choice (concat [parser1 parser2] parsers))))

(def || choice)

(defn optional [parser]
  (with-meta
    (choice parser epsilon)
    {:parser (str (parser-name parser) "?")}))

(defn one-of [chars]
  (apply choice (map #(match %) chars)))

(defn using [parser transformer]
  (with-meta
    (fn [inp]
      (let [r (apply-parser parser (assoc inp :result []))]
        (if (failure? r)
          r
          (succeed! (conj (:result inp) 
                          (-> r :result transformer)) 
                    r))))
    {:parser (str (parser-name parser) " [+ Transformer]")}))

(defn then
  ([] (with-meta epsilon {:parser "Epsilon"}))
  ([parser1] (with-meta parser1 (meta parser1)))
  ([parser1 parser2]
   (with-meta
     (fn [inp]
       (debug "Entering Then combinator with input " inp)
       (let [r1 (apply-parser parser1 inp)]
         (debug "Parser 1 [" (parser-name parser1) "] yielded " r1)
         (if (success? r1)
           (let [r2 (apply-parser parser2 (mask-result r1))]
             (debug "Parser 2 [" (parser-name parser2) "] yielded " r2)
             (if (success? r2)
               (succeed! (flatten (concat (:result r1)
                                          (:result r2)))
                        r2)
               (do
                 (debug "Parser 2 [" (parser-name parser2) "] failed, terminating chain.")
                 (fail r2))))
           (do
             (debug "Parser 1 [" (parser-name parser1) "] failed, terminating chain.")
             (fail r1)))))
     {:parser (str (parser-name parser1) " THEN " (parser-name parser2))}))
  ([parser1 parser2 & parsers] (fold then (cons parser1 (cons parser2 parsers)))))

(def |> then)

(defn literal [lit]
  (with-meta
    (apply then (map match lit))
    {:parser "Literal [" lit "]"}))

(defn one-or-more [parser]
  (then parser (star parser)))

(def plus one-or-more)
