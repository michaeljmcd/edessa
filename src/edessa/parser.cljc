(ns edessa.parser
  (:require [clojure.core.reducers :refer [fold]]
            [taoensso.timbre :as t :refer [debug error]]))

(defn fmt [& args]
  (let [f #?(:clj format
             :cljs goog.string.format)]
    (apply f args)))

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
  (let [res
    (cond
      (nil? v) (result inp)
      (sequential? v) 
        (if (empty? (result inp))
          (list v)
          (concat (result inp) v))
      :else (concat (result inp) (list v)))]
  (assoc inp :result res)))

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

(def success? 
  "A convenience function that accepts a parser state variable *r* and returns true if it represents a success state."
  (comp not failure?))

(defn alarm 
  "Accepts a parser *p* and an error handler *error-fn* and returns a parser that calls
  *p* and invokes *error-fn* on the result if it is a failure."
  [p err-fn]
  (if (nil? err-fn)
    p
    (fn [x]
      (let [r (apply-parser p x)]
        (if (failure? r)
          (err-fn r)
          r)))))

(defn using 
  "Returns a new parser that runs *transformer* on the result from *parser* on success."
  [parser transformer]
  (if (nil? transformer)
    parser
    (with-name
      (fn [inp]
        (let [r (apply-parser parser (assoc inp :result []))]
          (if (failure? r)
            r
            (succeed! (concat (:result inp)
                            (list (-> r :result transformer)))
                      r))))
      (str (parser-name parser) " [+ Transformer]"))))

(defn parser 
  "A function for declaring a parser, optionally adding an error function, a transformer and/or a name."
  [p & {:keys [error using name]}]
  (-> p
      (alarm error)
      (edessa.parser/using using)
      (with-name name)))

(defn discard 
  "A transformer that accepts a parser *p* and returns a version that discards its output while retaining a successful result."
  [p]
  (parser
   (fn [inp]
     (let [input-result (get inp :result)]
       (assoc (p inp)
              :result input-result)))
   :name (str (parser-name p) ", discarding output")))

(defn advance
  "Advances the parser state *inp* by incrementing position and adjusting line and column numbers appropriately."
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

(defn remaining 
  "Returns the unprocessed input in parser state *inp*."
  [inp]
  (get inp :input))

(defn look
  "Peeks at the next value in the input stream of parser state *inp* without advancing."
  [inp]
  (first (get inp :input)))

(def epsilon
  "An empty parser that returns no result and consumes no input."
  (parser identity :name "Epsilon (empty)"))

(defn fail
  "Returns a new instance of the state *inp* with failure indicated, optionally including a failure message to be indicated."
 {:parser "Fail"}
  ([inp]
   (if (nil? (:error inp))
     (fail inp "Parsing failed")
     (assoc inp :failed true)))
  ([inp message]
   (-> inp
       (assoc :error message)
       (assoc :failed true))))

(defn input-consumed?
  "Returns true if the parser state *r* has all input consumed."
  [r]
  (and (success? r)
       (empty? (get r :input))))

(def input-remaining? 
  "Returns true if the passed parser state has unprocessed input. The inverse of input-consumed?"
  (comp not input-consumed?))

(defn match
  "Accepts a value *c* and returns a parser that matches that single value.

  For example,
    
  ```
    (match \\newline)
  ```

  Returns a parser that will match a single newline."
  ([c]
   (parser
    (fn [inp]
      (if (and (input-remaining? inp)
               (= (look inp) c))
        (advance inp c)
        (fail inp (fmt "The value '%s' does not match the expected value of '%s'.", (look inp) c))))
    :name (str "Matches " c))))

(defn match-with
  "Accepts a predicate and returns a parser that matches input when that predicate returns true.

  For example,

  ```
    (match-with (fn [x] (odd? x)))
  ```

  Would return a parser that matches odd numbers in the input stream."
  [pred]
  (parser
   (fn [inp]
     (let [current (look inp)]
       (if (pred current)
         (advance inp current)
         (fail inp (fmt "The value %s does not satisfy the required conditions." current)))))
   :name (str "Match-with " pred)))

(defn not-one-of
  "Accepts a list of values (characters, for example) and returns a parser that matches any value in the list.

  For example,

  ```
    (not-one-of [\\a \\b \\c])
  ```

  Would match any character except 'a', 'b' or 'c'."
  [chars]
  (parser
   (fn [inp]
     (let [x (look inp)]
       (if (or (input-consumed? inp)
               (some (partial = x) chars))
         (fail inp (fmt "Value '%s' is not one of %s" x chars))
         (advance inp x))))
   :name (str "Not one of [" chars "]")))

(defn zero-or-more
  "A parser combinator that accepts a parser *p* and returns a parser that matches *p* zero or more times."
  [p]
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
                (concat (:result inp) (:result result))
                )))
     :name (str "Zero or more " (parser-name p)))))

(def star "Convenient alias for zero-or-more." zero-or-more)

(defn choice
  "Accepts zero or more parsers, returning the first successful parser's result."
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

(defn optional
  "Returns the result of parser `p` if successful and empty success if the parser fails. Corresponds to the ? operator."
  [p]
  (parser (choice p epsilon)
          :name (str (parser-name p) "?")))

(defn one-of
  "Creates a parser that succeeds if input matches any of the objects in `chars`."
  [chars]
  (apply choice (map #(match %) chars)))

(defn then
  "A combinator that accepts zero or more parsers and returns a parser that applies each parser successively against input."
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

(defn literal
  "Creates a parser that succeeds whenever the input matches `lit`."
  [lit]
  (parser (apply then (map match lit))
          :name (str "Literal [" lit "]")))

(defn one-or-more
  "A combinator that succeeds when `parser` matches one or more times. Corresponds to the + operator."
  [parser]
  (then parser (star parser)))

(def plus "An alias for `one-or-more`." one-or-more)

(defn trace
  "Trace wraps parser `p` in debug statements. Not recommended for production code, but useful for troubleshooting."
  [p]
  (let [name (parser-name p)]
    (parser
     (fn [x]
       (debug "Entering " parser-name " with: " x)
       (let [r0 (p x)]
         (debug parser-name "produced result: " r0)
         r0))
     :name (str name " [+Trace]"))))

(defn times
  "Accepts a parser `p` and matches it exactly `n` times. Equivalent to the `{n}` operator in regular expressions."
  [n p]
  (apply then (repeat n p)))
