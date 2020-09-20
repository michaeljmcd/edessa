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

(deftest core-combinators
 (let [p (zero-or-more (match \a))
       r0 (p "babc")]
  (is (success? r0))
  (is (input-remaining? r0))
  (is (= "babc" (remaining r0)))
  (is (success? (p "aaaa")))
  (is (= [] (remaining (p "aaaa"))))))
