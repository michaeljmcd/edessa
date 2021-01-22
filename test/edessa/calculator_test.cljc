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
                                    :value (read-string (apply str x))})
                 ))

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

(def number-token (parser (match-with #(= (:token %) :number))))
