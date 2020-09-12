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
  (is (= [[nil] "asdf"] r))))

(deftest metadata-tests
 (let [p (match \a)]
  (is (= "Matches a" (parser-name p)))))
