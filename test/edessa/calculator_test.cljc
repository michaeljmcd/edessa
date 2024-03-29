(ns edessa.calculator-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.match :as m]
            [clojure.core.reducers :as r]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info with-level with-merged-config spit-appender]]
            [clojure.java.io :as io]))

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

(def multiply (parser (match \*)
                   :using (fn [_] {:token :operator :value "*"})))

(def divide (parser (match \/)
                    :using (fn [_] {:token :operator :value "/"})))

(def operator (parser (choice plus1 minus multiply divide)))

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

(defn transform-term [components]
  (debug "Transform term " (pr-str components))
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
    (do
      (debug "transform-term, > 3 elements. Extra: " (pr-str ns))
      {:operator (operator-token->keyword op)
       :operands [n1 (transform-term ns)]})
    ([op :guard is-operator-token?
      n1
      n2] :seq)
    {:operator (operator-token->keyword op)
     :operands [n1 n2]}
    ([op] :seq) op))

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

(defn combine-subexpr
  ([] {})
  ([v1 v2]
   (debug "combine-subexpr: " v1 v2)
   (cond
     (= v1 {}) v2
     (and (= 1 (count (:operands v2)))
          (= 1 (count (:operands v1)))
          (= (:operator v1) (:operator v2)))
     {:operator (:operator v1) :operands (concat (:operands v1) (:operands v2))}
     (number? v1)
     (assoc v2
            :operands
            (cons v1 (:operands v2)))
     :else
     (assoc v2
            :operands
            (cons v1
                  (into [] (:operands v2)))))))

(defn create-subexpr-fragment [xs]
  {:operator (operator-token->keyword (first xs))
   :operands [(second xs)]})

(defn combine-fragments [xs]
  (debug "Folding " (pr-str xs))
  (r/fold combine-subexpr xs))

(def term (parser
           (then factor
                 (parser
                  (star
                   (choice
                    (trace (parser (then star-token factor)
                                   :using create-subexpr-fragment
                                   :name "Term branch - (* <factor>)"))
                    (parser (then slash-token factor)
                            :using create-subexpr-fragment
                            :name "Term branch - (/ <factor>)")))))

           :using combine-fragments

           :name "Term"))

(def expr (parser
           (then term
                 (star
                  (parser
                   (choice
                    (parser (then plus-token term)
                            :using create-subexpr-fragment)
                    (parser (then minus-token term)
                            :using create-subexpr-fragment))
                   :name "right-expr")))
           :using combine-fragments
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

(defn eval-calc [expr]
  (if (number? expr) expr
      (let [opr-list {:add #'+ :subtract #'- :multiply #'* :divide #'/}
            opr-fn (get opr-list (:operator expr))]
        (apply opr-fn (map eval-calc (:operands expr))))))

(defn eval-result [r]
  (eval-calc (first (result r))))

(deftest atomic-number-expression
  (with-level :error
    (let [input "39"
          r0 (parse-calc-text input)]
      (is (success? result))
      (is (= '[39] (result r0)))
      (is (= 39 (eval-result r0))))))

(deftest simple-addition-expression
  (with-level :error
    (let [input "1 + 1"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '({:operator :add :operands [1 1]})
             (result r0)))
      (is (= 2 (eval-result r0))))))

(deftest subtraction-with-negatives-expression
  (with-level :error
    (let [input "-1 - -1"
          r0 (parse-calc-text input)]
      (debug input " result: " r0)
      (is (success? r0))
      (is (= '[{:operator :subtract
                :operands [-1 -1]}]
             (result r0)))
      (is (= 0 (eval-result r0))))))

(deftest shorter-chain-expression
  (with-merged-config
    {:min-level :info
    ; :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}
     }
    (let [input "1 * 2 * 3"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply, :operands [{:operator :multiply, :operands [1 2]} 3]}]
             (result r0)))
      (is (= 6 (eval-result r0))))))

(deftest long-chain-expression
  (with-merged-config
    {:min-level :info
     :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}}
    (let [input "1 * 2 * 3*4"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply, :operands [{:operator :multiply
                                                 , :operands [{:operator :multiply, :operands [1 2]} 3]} 4]}]
             (result r0)))
      (is (= 24 (eval-result r0))))))

(deftest long-chain-expression2
  (with-merged-config
    {:min-level :info
     :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}}
    (let [input "1 + 2 + 3+4"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :add, :operands [{:operator :add
                                            , :operands [{:operator :add, :operands [1 2]} 3]} 4]}]
             (result r0)))
      (is (= 10 (eval-result r0))))))

(deftest long-multiply-and-divide-chain-expression
  (with-merged-config
    {:min-level :info
     :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}}
    (let [input "1 * 2 * 3*4/5"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :divide
                :operands [{:operator :multiply,
                            :operands [{:operator :multiply,
                                        :operands [{:operator :multiply,
                                                    :operands [1 2]}
                                                   3]} 4]}
                           5]}]
             (result r0)))
      (is (= 24/5 (eval-result r0))))))

(deftest shorter-chain-expression2
  (with-merged-config
    {:min-level :info}
    (let [input "1 * (2 + 3)"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply, :operands [1 {:operator :add, :operands [2 3]}]}]
             (result r0)))
      (is (= 5 (eval-result r0))))))

(deftest shorter-chain-expression3
  (with-merged-config
    {:min-level :info}
    (let [input "(1 * (2 - 3))"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply, :operands [1 {:operator :subtract, :operands [2 3]}]}]
             (result r0)))
      (is (= -1 (eval-result r0))))))

(deftest mixed-long-chain-expression
  (with-merged-config
    {:min-level :info
     :appenders {:spit (spit-appender {:fname "./timbre-spit.log"})}}
    (let [input "-300 * (4 * (111 * 5
               ) )  "
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [-300
                           {:operator :multiply,
                            :operands [4
                                       {:operator :multiply,
                                        :operands [111 5]}]}]}]
             (result r0)))
      (is (= -666000 (eval-result r0))))))

(deftest simple-multiplication
  (with-level :error
    (let [input "371 * 44"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply :operands [371 44]}]
             (result r0)))
      (is (= 16324 (eval-result r0))))))

(deftest chain-multiplication
  (with-level :error
    (let [input "100 * (200*300)"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [100
                           {:operator :multiply
                            :operands [200 300]}]}]
             (result r0)))
      (is (= 6000000 (eval-result r0))))))

(deftest chain-multiplication2
  (with-level :error
    (let [input "100 * (200*(300*400))"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [100
                           {:operator :multiply
                            :operands [200
                                       {:operator :multiply
                                        :operands [300 400]}]}]}]
             (result r0)))
      (is (= 2400000000 (eval-result r0))))))

(deftest parenthesized-expressions
  (with-level :error
    (let [input "(44.1 * 33)"
          r0 (parse-calc-text input)]
      (is (success? result))
      (is (= '[{:operator :multiply :operands [44.1 33]}]
             (result r0)))
      (is (= 1455.3 (eval-result r0))))))

(deftest simple-compound-expression
  (with-level :error
    (let [input "3 * (1 +2)"
          r0 (parse-calc-text input)]
      (is (success? r0))
      (is (= '[{:operator :multiply
                :operands [3 {:operator :add
                              :operands [1 2]}]}]
             (result r0)))
      (is (= 9 (eval-result r0))))))
