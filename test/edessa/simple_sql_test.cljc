(ns edessa.simple-sql-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [edessa.parser :refer :all]
            [taoensso.timbre :as t :refer [debug error info]]
            [clojure.java.io :as io]))

(def ws (with-meta 
          (star (one-of [\space \tab \newline]))
          {:parser "whitespace"}))

(def asterisk (match \*))

(def comma (match \,))

(def identifier 
  (parser 
    (plus (not-one-of [\" \tab \newline \, \space]))
    :using (fn [x] (info "Got " x) {:type :column :identifier (str/join x)})))

(def select-sublist
  (using
    (then
      identifier
      (star (then (discard (optional ws))
                  (discard comma) 
                  (discard (optional ws))
                  identifier)))
    (fn [x] 
      (filter (comp not nil?) x)
    )))

(def select-list select-sublist)

(def select-statement 
  (using
    (then
      (discard (optional ws))
      (discard (literal "SELECT"))
      (discard (optional ws))
      (choice
        select-list
        (using asterisk (fn [_] {:type :column-list :columns :all}))))
    (fn [x] 
      (info "Got " (pr-str x)) 
      {:command :select :columns (filter (comp not nil?) x)})
    ))

(def sql select-statement)

(defn test-resource [n]
  (-> n io/resource slurp))

(deftest select-test
  (let [text (test-resource "simple-select.sql")
        r0 (apply-parser sql text)]
    (info "Got result: " r0)))

(deftest error-test
  (let [text (test-resource "invalid-select.0.sql")
        r0 (apply-parser sql text)]
    (is (failure? r0))
    (info "Got result: " r0)))
