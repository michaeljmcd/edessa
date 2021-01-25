(ns edessa.calculator-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info]]
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

(def times (parser (match \*)
                   :using (fn [_] {:token :operator :value "*"})))

(def divide (parser (match \/)
                    :using (fn [_] {:token :operator :value "/"})))

(def operator (parser (choice plus1 minus times divide)))

(def number (parser (then 
                      (optional (match \-)) 
                      digits 
                      (optional (then dot digits)))
                    :using (fn [x] (info "Number parsing " (apply str (filter (comp not nil?) x)))
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

(def number-token (parser (match-with #(= (:token %) :number))
                          :using (fn [x] (info "Number token " (pr-str x)) (:value (first x)))))

(def operator-token (parser (match-with #(= (:token %) :operator))))

(def left-paren-token (parser (match-with #(= (:token %) :open-parentheses))))

(def right-paren-token (parser (match-with #(= (:token %) :close-parentheses))))


(declare expr)

(def simple-expr (parser (choice (then edessa.calculator-test/expr operator-token edessa.calculator-test/expr) number-token)
                             :using (fn [x] 
                                      (info "Got " (pr-str x))
                                      (let [components (filter (comp not nil?) x)]
                                        (if (= 1 (count components))
                                          (first components)
                                          {:operator (second components) :operands [(first components) (nth components 2)]})))))

(def paren-expr (parser (then left-paren-token simple-expr right-paren-token)))

(def expr (parser (choice paren-expr simple-expr)))
