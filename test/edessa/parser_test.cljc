(ns edessa.parser-test
 (:require [clojure.test :refer :all]
           [edessa.parser :refer :all]))

(deftest ground-truths 
 (is (= [] (fail "asdf")))
 (is (failure? (fail 33333)))

 (is (= [[1] "asdf"] (succeed 1 "asdf")))
 (is (= [[[1]] "asdf"] (succeed [1] "asdf")))

 (is (success? (succeed 88 nil)))
 (is (failure? (fail 22)))

 (is (not (success? (fail 22))))
 (is (not (failure? (succeed 88 "asdf"))))

 (is (not (input-consumed? (fail 22))))
 (is (input-consumed? (succeed 2 nil)))

 (is (not (input-remaining? (succeed 2 nil)))))

(deftest basic-building-blocks
 (let [r (epsilon "asdf")]
  (is (success? r))
  (is (= "asdf" (remaining r)))
  (is (= [[nil] "asdf"] r)))

 (let [p (match \a)]
  (is (success? (p "aaa")))
  (is (= [\a] (result (p "aaa"))))
  (is (= [\a \a] (remaining (p "aaa"))))
  (is (input-remaining? (p "aaa"))))

 (let [p (match \a)]
  (is (failure? (p "baa")))
  (is (= [] (p "baa"))))

 (let [p (not-one-of [\a \b \c])
       r (p "dabc")]
  (is (success? r))
  (is (= [\a \b \c] (remaining r)))
  (is (input-remaining? r))
  (is (failure? (p "adabc")))))

(deftest metadata-tests
 (let [p (match \a)]
  (is (= "Matches a" (parser-name p)))))

(deftest star-combinator
 (let [p (zero-or-more (match \a))
       r0 (p "babc")]
  (is (success? r0))
  (is (input-remaining? r0))
  (is (= "babc" (remaining r0)))
  (is (success? (p "aaaa")))
  (is (= [] (remaining (p "aaaa"))))))

(deftest choice-combinator
 (let [p (choice (match \a) (match \b))
       r0 (p "baa")
       r1 (p "abb")
       r2 (p "cab")]
   (is (success? r0))
   (is (= [\b] (result r0)))
   (is (= [\a \a] (remaining r0)))

   (is (success? r1))
   (is (= [\a] (result r1)))
   (is (= [\b \b] (remaining r1)))

   (is (failure? r2)))

 (let [p (choice)]
  (is (failure? (p "aaa"))))

 (let [p (choice (match \a))
       r0 (p "ab")
       r1 (p "ba")]
  (is (success? r0))
  (is (= [\b] (remaining r0)))
  (is (= [\a] (result r0)))

  (is (failure? r1)))

  (let [p (choice (match \a) (match \b) (match \c) (match \d))
        r0 (p "czz")
        r1 (p "fgh")]
    (is (success? r0))
    (is (= [\c] (result r0)))
    (is (= [\z \z] (remaining r0)))

    (is (failure? r1))))

(deftest optional-combinator
 (let [p (optional (match \a))
       r0 (p "abc")
       r1 (p "dabc")]
    (is (success? r0))
    (is (= [\a] (result r0)))
    (is (= [\b \c] (remaining r0)))

    (is (success? r1))
    (is (= [nil] (result r1)))
    (is (= "dabc" (remaining r1)))))
