(ns edessa.calculator-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.match :as m]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info with-level with-merged-config spit-appender]]
            [clojure.java.io :as io]))

(def not-nil? (comp not nil?))

; The general idea is to do a two pass compiler on a simple calculator for no
; other reason than to provide a different take on parsing for test purposes.

(def ws (parser (star (one-of [\space \tab \newline]))
                :name "Whitespace"))

(def digits (parser (plus (one-of [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9]))
                    :name "Digits"))

(def dot (parser (match \.)))

(def plus1 (parser (match \+)
                   :using (fn [_] {:token :operator :value "+"})))

(def minus (parser (match \-)
                   :using (fn [_] {:token :operator :value "-"})))

(def times (parser (match \*)
                   :using (fn [_] {:token :operator :value "*"})))

(def divide (parser (match \/)
                    :using (fn [_] {:token :operator :value "/"})))

(def operator (parser (choice plus1 minus times divide)))

(def number (parser (then
                     (optional (match \-))
                     digits
                     (optional (then dot digits)))
                    :using (fn [x]
                             {:token :number
                              :value (read-string (apply str x))})))

(def left-paren (parser (match \()
                        :using (fn [_] {:token :open-parentheses :value "("})))

(def right-paren (parser (match \))
                         :using (fn [_] {:token :close-parentheses :value ")"})))

(def token (choice
            left-paren
            right-paren
            number
            operator
            (discard ws)))

(def tokens (plus token))

; With the tokenization done, we turn to recognizing a stream of token objects.
; We want a grammar that looks like this:
;
; Expr ::= Term ('+' Term | '-' Term)*
; Term ::= Factor ('*' Factor | '/' Factor)*

; Factor ::= ['-'] (Number | '(' Expr ')')

; Number ::= Digit+
;
; Source: https://stackoverflow.com/questions/9785553/how-does-a-simple-calculator-with-parentheses-work

(defn is-left-paren-token? [x] (= (:token x) :open-parentheses))

(defn is-right-paren-token? [x] (= (:token x) :close-parentheses))

(def left-paren-token (parser (match-with is-left-paren-token?)
                              :name "Left Paren"))

(def right-paren-token (parser (match-with is-right-paren-token?)
                               :name "Right Paren"))

(def is-numbert? (fn [x] (= (:token x) :number)))

(def number-token (parser (match-with is-numbert?)
                          :name "Number"
                          :using (fn [x]
                                   (debug "number-token " x)
                                   (:value (first x)))))

(def plus-token (parser (match {:token :operator :value "+"})
                        :name "Plus Operator"))

(defn minus-token? [x]
  (= x {:token :operator :value "-"}))

(def minus-token (parser (match-with minus-token?)
                         :name "Minus Operator"))

(def star-token (parser (match {:token :operator :value "*"})
                        :name "Multiply Operator"))

(def slash-token (parser (match {:token :operator :value "/"})
                         :name "Divide Operator"))

(defn is-operator-token? [x] (= (:token x) :operator))

(def operator-token (parser (match-with is-operator-token?)))

(defn operator-token->keyword [t]
  (debug "Operator token " t)
  (case (:value t)
    "*" :multiply
    "+" :add
    "-" :subtract
    "/" :divide))

(declare expr)

(defn transform-term [x]
  (info "Transform term " (pr-str x))
  (let [components (filter not-nil? x)]
    (m/match components
      ([n :guard number?] :seq) n
      ([_ :guard is-left-paren-token?
        e
        _ :guard is-right-paren-token?] :seq) e
      ([n1
        op :guard is-operator-token?
        n2] :seq)
      {:operator (operator-token->keyword op)
       :operands [(transform-term [n1])
                  (transform-term [n2])]}
      ([n1
        op :guard is-operator-token?
        & ns] :seq)
      {:operator (operator-token->keyword op)
       :operands [n1 (transform-term ns)]}
      ([op :guard is-operator-token?
        n1
        n2] :seq)
      {:operator (operator-token->keyword op)
       :operands [n1 n2]}
      ([op] :seq) op)))

(def factor (parser
             (then
              (choice
               number-token
               (parser
                 (then
                  left-paren-token
                  #'expr
                  right-paren-token)
                 :name "Parenthesized Expr")))
             :using transform-term
             :name "Factor"))

(def term (parser
           (then factor
                 (star
                  (choice
                   (then star-token factor)
                   (then slash-token factor))))
           :using transform-term
           :name "Term"))

(def expr (parser
           (then term
                 (star
                  (parser
                   (choice
                    (then plus-token term)
                    (then minus-token term))
                   :name "right-expr")))
           :using transform-term
           :name "Expr"))

(defn log-result [x stage]
  (debug stage " result => " x)
  x)

(defn parse-calc-text [input]
  (-> input
      make-input
      tokens
      (log-result "Tokens")
      result
      make-input
      expr
      (log-result "Parse")))

(deftest atomic-number-expression
  (with-level :info
    (let [input "39"
          r0 (parse-calc-text input)]
      (is (success? result))
      (is (= '[39] (result r0))))))

(deftest simple-addition-expression
  (with-level :info
    (let [input "1 + 1"
          r0 (parse-calc-text input)]
      (debug "1 + 1 result: " (pr-str result))
      (is (success? r0))
      (is (= '({:operator :add :operands [1 1]})
             (result r0))))))

(deftest subtraction-with-negatives-expression
  (with-level :info
    (let [input "-1 - -1"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :subtract
                :operands [-1 -1]}]
             (result r0))))))

(deftest shorter-chain-expression
(with-merged-config
    {:min-level :debug
    ; :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}
     }
    (let [input "1 * 2 * 3"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply, 
                :operands [1
                           {:operator :multiply
                            :operands [2 3]}]}]
             (result r0))))))

(deftest shorter-chain-expression2
(with-merged-config
    {:min-level :debug
    ; :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}
     }
    (let [input "1 * 2 + 3"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :add 
                :operands [{:operator :multiply
                            :operands [1 2]}
                           3]}]
             (result r0))))))

(deftest shorter-chain-expression3
(with-merged-config
    {:min-level :debug
    ; :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}
     }
    (let [input "1 * 2 - 3"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :subtract 
                :operands [{:operator :multiply
                            :operands [1 2]}
                           3]}]
             (result r0))))))

(deftest long-chain-expression
  (with-merged-config
    {:min-level :debug
     :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}}
    (let [input "-300 * 4 * 111 * 5 + 1 * 3 - 2"
          r0 (parse-calc-text input)]
      (info input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :subtract, 
                :operands [{:operator :multiply, 
                            :operands [-300 
                                       {:operator :multiply, 
                                        :operands [4 
                                                   {:operator :multiply, 
                                                    :operands [111 4]}]}]} 
                           {:operator :add, :operands [{:operator :multiply, 
                                                        :operands [1 3]} 
                                                       1]}]}]
             (result r0))))))

(deftest simple-multiplication
  (with-level :info
    (let [input "371 * 44"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :multiply :operands [371 44]}]
             (result r0))))))

(deftest chain-multiplication
  (with-level :info
    (let [input "100 * 200*300"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [100
                           {:operator :multiply
                            :operands [200 300]}]}]
             (result r0))))))

(deftest chain-multiplication2
  (with-level :info
    (let [input "100 * (200*300)"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [100
                           {:operator :multiply
                            :operands [200 300]}]}]
             (result r0))))))

(deftest chain-multiplication3
  (with-level :info
    (let [input "100 * (200*(300*400))"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [100
                           {:operator :multiply
                            :operands [200
                                       {:operator :multiply
                                        :operands [300 400]}]}]}]
             (result r0))))))

(deftest parenthesized-expressions
  (with-level :info
    (let [input "(44.1 * 33)"
          r0 (parse-calc-text input)]
      (debug input " result: " result)
      (is (success? result))
      (is (= '[{:operator :multiply :operands [44.1 33]}]
             (result r0))))))

(deftest simple-compound-expression
  (with-level :info
    (let [input "3 * (1 +2)"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [3 {:operator :add
                              :operands [1 2]}]}]
             (result r0))))))
