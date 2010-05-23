(ns gremlin.contrib.xpath.util-test
  (:use [clojure.test]
        [gremlin.contrib.xpath.util])
  (:import [org.apache.commons.jxpath.ri Parser]
           [org.apache.commons.jxpath.ri.compiler Step Constant TreeCompiler]))

(deftest get-axis-test
  (let [compiler (TreeCompiler.)
        xpath (Parser/parseExpression "./outE/@name" compiler)
        steps (seq (.getSteps xpath))]
    (is (= (map get-axis steps) '("self" "child" "attribute")))))

(deftest skip-quotes-test
  (is (= (skip-quotes "'friend'") "friend")))

(deftest skip-spaces-test
  (is (= (skip-spaces "no spaces") "nospaces")))

(deftest xpath-constant-value-test
  (let [num-constant (Constant. 10.5)
        str-constant (Constant. "'hello'")
        str-num-constant (Constant. "'10.5'")]
    (is (= (xpath-constant-value num-constant) 10.5))
    (is (= (xpath-constant-value str-constant) "hello"))
    (is (= (xpath-constant-value str-num-constant) "10.5"))))

