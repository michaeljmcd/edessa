(ns edessa.calculator-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.core.match :as m]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info with-level]]
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
                    :using (fn [x] (info "Number parsing " (apply str (filter not-nil? x)))
                             {:token :number 
                                    :value (read-string (apply str x))})))

(def left-paren (parser (match \( )
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

(defn is-left-paren-token? [x] (= (:token x) :open-parentheses))

(def left-paren-token (parser (match-with is-left-paren-token?)))

(def right-paren-token (parser (match-with (fn [x] (= (:token x) :close-parentheses)))))

(def is-numbert? (fn [x] (= (:token x) :number)))

(def number-token (parser (match-with is-numbert?)
                          :using (fn [x] (info "Number token " (pr-str x)) (:value (first x)))))

(def plus-token (parser (match {:token :operator :value "+"})))

(defn minus-token? [x]
  (= x {:token :operator :value "-"}))

(def minus-token (parser (match-with minus-token?)))

(def star-token (parser (match {:token :operator :value "*"})))

(def slash-token (parser (match {:token :operator :value "/"})))

(defn is-operator-token? [x] (= (:token x) :operator))

(def operator-token (parser (match-with is-operator-token?)))

(defn operator-token->keyword [t]
  (case (:value t)
    "*" :multiply
    "+" :add
    "-" :subtract
    "/" :divide))

(declare expr)

(defn transform-factor [x]
   (let [components (filter not-nil? x)
         nextval (first components)]
   (info "factor: " (pr-str components))
   (cond
     (number? nextval) nextval
     (minus-token? nextval)
      {:operator :multiply
       :operands [-1 (transform-factor (rest components))]} ; Let me get it working first, then I'll clean it up.
      (is-numbert? nextval)
       (:value nextval)
     (is-left-paren-token? nextval)  ; parenthesized expression
      (transform-factor (subvec (into [] components) 1 (- (count components) 1)))
     (= (count components) 3) ; presumably a simple <val> operand <val> expr
      {:operator (-> components (nth 1) operator-token->keyword)
       :operands [(transform-factor [(first components)])
                  (transform-factor [(nth 2 components)])]}
      :else nextval
    )
     )
   )

(def factor (parser
              (then
                (optional minus-token)
                (choice
                  number-token
                  (then 
                    left-paren-token
                    #'expr
                    right-paren-token)))
              :using transform-factor))

(defn transform-term [x]
  (info "Transform term " (pr-str x))
  (m/match x
           ([n :guard number?] :seq) n
            ([n1 op n2] :seq)
            {:operator (operator-token->keyword op)
             :operands [(transform-term [n1])
                        (transform-term [n2])]}
           :else x
           ))

(def term (parser
            (then factor
                (star
                  (choice 
                    (then star-token factor)
                    (then slash-token factor))))
            :using transform-term))

(defn transform-expr [x]
                (let [components (filter not-nil? x)]
                  (info "expr components " (pr-str components))
                  (m/match components
                           ([n1 op n2] :seq)
                            {:operator (operator-token->keyword op)
                             :operands [n1 n2]}
                           :else components
                           )
                ))

(def expr (parser
            (then term 
                (star
                  (choice
                    (then plus-token term)
                    (then minus-token term))))
            :using transform-expr
              ))

(defn parse-calc-text [input]
  (-> input
        make-input
        tokens
        result
        make-input
        expr))

(deftest atomic-number-expression
  (with-level :info
    (let [input "39"
          r0 (parse-calc-text input)]
      (is (success? result))
      (is (= '[(39)] (result r0)))
      )))

(deftest simple-addition-expression
  (with-level :info
  (let [input "1 + 1"
    r0 (parse-calc-text input)]
    (info "1 + 1 result: " (pr-str result))
    (is (success? r0))
    (is (= '({:operator :add :operands [1 1]})
           (result r0)))
    )))

(deftest subtraction-with-negatives-expression
  (with-level :info
  (let [input "-1 - -1"
        r0 (parse-calc-text input)]
    (info input " result: " r0)
    (is (success? r0))
    (is (= '[{:operator :subtract
              :operands [-1 -1]}]
           (result r0)))
  )))

(deftest long-chain-expression
  (with-level :info
  (let [input "-300 * 4 * 111 * 4 + 1 * 3 - 1"
        result (parse-calc-text input)]
    (is (success? result))
    (info input " result: " result))
  ))
  
(deftest simple-valid-expressions
  (with-level :info
  (let [input "371 * 44"
        r0 (parse-calc-text input)]
    (info "371 * 44 result: " r0)
    (is (success? r0))
    (is (= '[({:operator :multiply :operands [371 44]})]
           (result r0)))
  )))

(deftest parenthesized-expressions
  (with-level :info
  (let [input "(44.1 * 33)"
        result (parse-calc-text input)]
    (is (success? result))
    (info input " result: " result))))
  
(deftest simple-compound-expression
  (with-level :info
  (let [input "3 * (1 +2)"
        r0 (parse-calc-text input)]
    (is (success? r0))
    (info input " result: " r0))
  ))
