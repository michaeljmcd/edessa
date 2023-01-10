(ns edessa.parser-test
  (:require [clojure.test :refer :all]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info]]))

(deftest ground-truths
  (is (= {:input "asdf" :position 0 :line-number 0 :column 0 :result [] :failed true :error "Nope"}
         (fail (make-input "asdf") "Nope")))
  (is (failure? (fail (make-input 33333))))

  (is (= {:input "asdf", :position 0, :line-number 0, :column 0, :result [1], :failed false, :error nil}
         (succeed 1 (make-input "asdf"))))
  (is (= {:input "asdf", :position 0, :line-number 0, :column 0, :result [[1]], :failed false, :error nil}
         (succeed [1] (make-input "asdf"))))

  (is (success? (succeed 88 (make-input nil))))
  (is (failure? (fail (make-input 22))))

  (is (not (success? (fail (make-input 22)))))
  (is (not (failure? (succeed 88 (make-input "asdf")))))

  (is (not (input-consumed? (fail (make-input 22)))))
  (is (input-consumed? (succeed 2 nil)))

  (is (not (input-remaining? (succeed 2 nil)))))

(deftest basic-building-blocks
  (let [r (apply-parser epsilon "asdf")]
    (is (success? r))
    (is (= "asdf" (remaining r)))
    (is (= '{:input "asdf", :position 0, :line-number 0, :column 0, :result [], :failed false, :error nil}
           r)))

  (let [p (match \a)
        r0 (apply-parser p "aaa")]
    (is (success? r0))
    (is (= [\a] (result r0)))
    (is (= [\a \a] (remaining r0)))
    (is (input-remaining? r0)))

  (let [p (match \a)]
    (is (failure? (apply-parser p "baa")))
    (is (= {:input "baa", :position 0, :line-number 0, :column 0, :result [], :failed true, :error "The value 'b' does not match the expected value of 'a'."}
           (apply-parser p "baa"))))

  (let [p (not-one-of [\a \b \c])
        r (apply-parser p "dabc")]
    (is (success? r))
    (is (= [\a \b \c] (remaining r)))
    (is (input-remaining? r))
    (is (failure? (apply-parser p "adabc")))))

(deftest metadata-tests
  (let [p (match \a)]
    (is (= "Matches a" (parser-name p)))))

(deftest star-combinator
  (let [p (zero-or-more (match \a))
        r0 (apply-parser p "babc")]
    (is (success? r0))
    (is (input-remaining? r0))
    (is (= "babc" (remaining r0)))
    (is (success? (apply-parser p "aaaa")))

    (info (remaining (apply-parser p "aaaa")))
    (is (input-consumed? (apply-parser p "aaaa")))))

(deftest choice-combinator
  (let [p (choice (match \a) (match \b))
        r0 (apply-parser p "baa")
        r1 (apply-parser p "abb")
        r2 (apply-parser p "cab")]
    (is (success? r0))
    (is (= [\b] (result r0)))
    (is (= [\a \a] (remaining r0)))

    (is (success? r1))
    (is (= [\a] (result r1)))
    (is (= [\b \b] (remaining r1)))

    (is (failure? r2)))

  (let [p (choice)]
    (is (failure? (apply-parser p "aaa"))))

  (let [p (choice (match \a))
        r0 (apply-parser p "ab")
        r1 (apply-parser p "ba")]
    (is (success? r0))
    (is (= [\b] (remaining r0)))
    (is (= [\a] (result r0)))

    (is (failure? r1)))

  (let [p (choice (match \a) (match \b) (match \c) (match \d))
        r0 (apply-parser p "czz")
        r1 (apply-parser p "fgh")]
    (is (success? r0))
    (is (= [\c] (result r0)))
    (is (= [\z \z] (remaining r0)))

    (is (failure? r1))))

(deftest optional-combinator
  (let [p (optional (match \a))
        r0 (apply-parser p "abc")
        r1 (apply-parser p "dabc")]
    (is (success? r0))
    (is (= [\a] (result r0)))
    (is (= [\b \c] (remaining r0)))

    (is (success? r1))
    (is (= [] (result r1)))
    (is (= "dabc" (remaining r1)))))

(deftest one-of-operator
  (let [p (one-of [\a \b \c])
        r0 (apply-parser p "cde")
        r1 (apply-parser p "zzz")]
    (is (success? r0))
    (is (= [\d \e] (remaining r0)))
    (is (= [\c] (result r0)))

    (is (failure? r1))))

(deftest using-combinator
  (let [p (using (match \a) (fn [x] {:result x}))
        r0 (apply-parser p "abc")
        r1 (apply-parser p "dabc")]
    (is (success? r0))
    (is (= [\b \c] (remaining r0)))
    (is (= [{:result [\a]}] (result r0)))

    (is (failure? r1))))

(deftest contextually-using-combinator
  (let [p (contextually-using (match \a) (fn [c x] {:result x :line (:line-number c)}))
          r0 (apply-parser p "abc")
          r1 (apply-parser p "dabc")]
      (is (success? r0))
      (is (= [\b \c] (remaining r0)))
      (is (= [{:result [\a] :line 0}] (result r0)))

      (is (failure? r1))))

(deftest then-combinator
  (let [p (then)
        r0 (apply-parser p "asdf")]
    (is (success? r0))
    (is (= "asdf" (remaining r0)))
    (is (= [] (result r0))))

  (let [p (then (match \a))
        r0 (apply-parser p "asdf")]
    (is (success? r0))
    (is (= [\s \d \f] (remaining r0)))
    (is (= [\a] (result r0))))

  (let [p (then (match \a) (match \s))
        r0 (apply-parser p "asdf")]
    (is (success? r0))
    (is (= [\d \f] (remaining r0)))
    (is (= [\a \s] (result r0))))

  (let [p (then (match \a) (match \s) (match \d) (match \f))
        r0 (apply-parser p "asdf")]
    (is (success? r0))
    (is (= [] (remaining r0)))
    (is (= [\a \s \d \f] (result r0)))))

(deftest literal-operator
  (let [p (literal "aa")
        r0 (p "aaa")]
    (is (success? r0))
    (is (= [\a] (remaining r0)))
    (is (= [\a \a] (result r0)))))

(deftest plus-combinator
  (let [p (plus (match \a))
        r0 (p "aaaab")]
    (is (success? r0))
    (is (= [\a \a \a \a] (result r0)))
    (is (= [\b] (remaining r0)))))

(deftest discard-combinator
  (let [p (then (match \a) (discard (match \b)))
        r0 (apply-parser p "ab")]
    (is (success? r0))
    (is (= [\a] (result r0)))
    (is (input-consumed? r0))))

(deftest match-with-combinator
  (let [p (match-with number?)
        r0 (apply-parser p [300])]
    (is (success? r0))
    (is (= '[300] (result r0)))
    (is (input-consumed? r0))))

(deftest trace-combinator
  (let [p (trace (match \a))
        r0 (apply-parser p "abc")]
    (is (success? r0))
    (is (= '[\a] (result r0)))
    (is (not (input-consumed? r0)))))

(deftest simple-singleline-error
  (let [p (then (star (match \a))
                (match \b))
        r0 (apply-parser p "aaa")]
    (is (failure? r0))
    (is (= '{:input ()
             :position 3
             :line-number 0
             :column 3
             :result []
             :error "The value 'null' does not match the expected value of 'b'."
             :failed true}
           r0))))

(deftest simple-multiline-error
  (let [p (then (star (match \a))
                (match \newline)
                (match \z))
        r0 (apply-parser p "aaaaaaaa\nb")]
    (is (failure? r0))
    (is (= '{:input (\b),
             :position 9,
             :line-number 1,
             :column 0,
             :result [],
             :error "The value 'b' does not match the expected value of 'z'.",
             :failed true}
           r0))))

(deftest simple-custom-error-message
  (let [p (parser (then (star (match \a))
                        (match \b))
                  :error (fn [x] (assoc x :error "Unicorns")))
        r0 (apply-parser p "aaa")]
    (is (failure? r0))
    (is (= '{:input ()
             :position 3
             :line-number 0
             :column 3
             :result []
             :error "Unicorns"
             :failed true}
           r0))))

(deftest match-times-test
  (let [p (parser (times 3 (match \a)))
        r (apply-parser p "aaa")]
    (is (success? r))
    (is (= '{:input ()
             :position 3
             :line-number 0
             :column 3
             :result [\a \a \a]
             :error nil
             :failed false}
           r))))
