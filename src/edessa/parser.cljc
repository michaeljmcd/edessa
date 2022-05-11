(ns edessa.parser
  (:require [clojure.core.reducers :refer [fold]]
            [taoensso.timbre :as t :refer [debug error]]))

; General parsing functions and combinators.

(defn make-input 
  "Accepts a sequence of objects *inp* (generally a string) and wraps it 
   to create an object representing parser state. This wrapped
   value includes the remaining input, position, line number, result and error
   handling information."
  [inp]
  {:input inp
   :position 0
   :line-number 0
   :column 0
   :result []
   :failed false
   :error nil})

(defn mask-result
  "Accepts returns a copy of a parser state object *inp* with the result
   removed."
  [inp]
  (assoc inp :result []))

(defn apply-parser 
  "A convenience function that applies a parser *p* to input *inp*.
   If *inp* is not a map, it will first be passed to *make-input* to create
   a valid parser state object."
  [p inp]
  (if (map? inp)
    (p inp)
    (p (make-input inp))))

(defn result 
  "A convenience function to return the result from a parser state object *inp*."
  [inp]
  (get inp :result))

(defn succeed 
  "Accepts a value *v* and a parser state object *inp* and returns an
   an updated parser state with *v* appended to the result of *inp*."
  {:parser "Succeed"} 
  [v inp]
  (assoc inp :result (conj (result inp) v)))

(defn succeed!
  "Accepts a value *v* and a parser state *inp* and returns *inp*
   with the result replaced with *v*."
  {:parser "Succeed!"}
  [v inp]
  (assoc inp :result v))

(defn parser-name 
  "A convenience function to return the name of a parser function from metadata."
  [parser] 
  (-> parser meta :parser))

(defn with-name 
  "Accepts a parser *p* and returns a copy of that parser with the parser name set to *name*."
  [p name]
  (if (nil? name)
    p
    (let [orig-meta (meta p)]
      (with-meta p (assoc orig-meta :parser name)))))

(defn failure? 
  "A convenience function that returns a truthy value if the parser state *r* is in a failure state."
  [r]
  (get r :failed))

(def success? (comp not failure?))

(defn alarm [p err-fn]
  (if (nil? err-fn)
    p
    (fn [x]
      (let [r (apply-parser p x)]
        (if (failure? r)
          (err-fn r)
          r)))))

(defn using [parser transformer]
  (if (nil? transformer)
    parser
    (with-name
      (fn [inp]
        (let [r (apply-parser parser (assoc inp :result []))]
          (if (failure? r)
            r
            (succeed! (conj (:result inp)
                            (-> r :result transformer))
                      r))))
      (str (parser-name parser) " [+ Transformer]"))))

(defn parser [p & {:keys [error using name]}]
  (-> p
      (alarm error)
      (edessa.parser/using using)
      (with-name name)))

(defn discard [p]
  (parser
   (fn [inp]
     (let [input-result (get inp :result)]
       (assoc (p inp)
              :result input-result)))
   :name (str (parser-name p) ", discarding output")))

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
  ([inp v] (advance (succeed v inp))))

(defn remaining [inp] (get inp :input))

(defn look [inp]
  (first (get inp :input)))

(def epsilon
  (parser identity :name "Epsilon (empty)"))

(defn fail {:parser "Fail"}
  ([inp]
   (if (nil? (:error inp))
     (fail inp "Parsing failed")
     (assoc inp :failed true)))
  ([inp message]
   (-> inp
       (assoc :error message)
       (assoc :failed true))))

(defn input-consumed? [r]
  (and (success? r)
       (empty? (get r :input))))

(def input-remaining? (comp not input-consumed?))

(defn match
  ([c]
   (parser
    (fn [inp]
      (if (and (input-remaining? inp)
               (= (look inp) c))
        (advance inp c)
        (fail inp (format "The value '%s' does not match the expected value of '%s'.", (look inp) c))))
    :name (str "Matches " c))))

(defn match-with [pred]
  (parser
   (fn [inp]
     (let [current (look inp)]
       (if (pred current)
         (advance inp current)
         (fail inp (format "The value %s does not satisfy the required conditions." current)))))
   :name (str "Match-with " pred)))

(defn not-one-of [chars]
  (parser
   (fn [inp]
     (let [x (look inp)]
       (if (or (input-consumed? inp)
               (some (partial = x) chars))
         (fail inp (format "Value '%s' is not one of %s" x chars))
         (advance inp x))))
   :name (str "Not one of [" chars "]")))

(defn zero-or-more [p]
  (letfn [(accumulate [inp]
            (debug "Z*: " (parser-name parser) " Input: " inp)
            (if (input-consumed? inp)
              inp
              (let [r (p inp)]
                (debug "Z*: Parser " (parser-name p) " yielded " (str r))
                (if (failure? r)
                  (do
                    (debug "Z*: Hit end of matches, returning " inp)
                    inp)
                  (recur r)))))]
    (parser
     (fn [inp]
       (let [result (accumulate (assoc inp :result []))]
         (assoc result
                :result
                (conj (:result inp) (:result result))
                )))
     :name (str "Zero or more " (parser-name p)))))

(def star zero-or-more)

(defn choice
  ([]
   (parser fail :name "Fail"))
  ([parser1]
   (parser parser1 :name (parser-name parser1)))
  ([parser1 parser2]
   (parser
    (fn [inp]
      (let [r1 (parser1 inp)]
        (if (failure? r1)
          (parser2 inp)
          r1)))
    :name (str (parser-name parser1) " OR " (parser-name parser2))))
  ([parser1 parser2 & parsers]
   (fold choice (concat [parser1 parser2] parsers))))

(defn optional [p]
  (parser (choice p epsilon)
          :name (str (parser-name p) "?")))

(defn one-of [chars]
  (apply choice (map #(match %) chars)))

(defn then
  ([] (parser epsilon :name "Epsilon"))
  ([parser1] (parser parser1 :name (parser-name parser1)))
  ([parser1 parser2]
   (let [p1-name (parser-name parser1)
         p2-name (parser-name parser2)
         parser-name (str (parser-name parser1) " THEN " (parser-name parser2))]
     (parser
      (fn [inp]
        (debug "Entering Then combinator (" parser-name ") with input " inp)
        (let [r1 (apply-parser parser1 inp)]
          (debug "Parser 1 [" p1-name "] yielded " r1)
          (if (success? r1)
            (let [r2 (apply-parser parser2 (mask-result r1))]
              (debug "Parser 2 [" p2-name "] yielded " r2)
              (if (success? r2)
                (succeed! (flatten (concat (:result r1)
                                           (:result r2)))
                          r2)
                (do
                  (debug "Parser 2 [" p2-name "] failed with " r2 ", terminating chain.")
                  (fail r2))))
            (do
              (debug "Parser 1 [" p1-name "] failed with " r1 ", terminating chain.")
              (fail r1)))))
      :name parser-name)))
  ([parser1 parser2 & parsers] (fold then (cons parser1 (cons parser2 parsers)))))

(defn literal [lit]
  (parser (apply then (map match lit))
          :name (str "Literal [" lit "]")))

(defn one-or-more [parser]
  (then parser (star parser)))

(def plus one-or-more)

(defn trace [p]
  (let [name (parser-name p)]
    (parser
     (fn [x]
       (debug "Entering " parser-name " with: " x)
       (let [r0 (p x)]
         (debug parser-name "produced result: " r0)
         r0))
     :name (str name " [+Trace]"))))
