(ns gremlin.contrib.xpath.compiler-test
  (:use [clojure.test]
        [gremlin.contrib.xpath.compiler])
  (:import [com.tinkerpop.pipes.serial Pipeline]
           [com.tinkerpop.blueprints.pgm.impls.tg TinkerGraphFactory]))

(defonce graph (TinkerGraphFactory/createTinkerGraph))
(defonce vertex (.getVertex graph 1))

(deftest compile-to-pipeline-test
  (is (= (.getClass (compile-xpath "./outE/inV/@name")) Pipeline)))

(deftest xpath-results-without-predicates-test
  (let [pipeline (compile-xpath "./outE/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= (.size results) 3))
      (is (= results '("vadas" "lop" "josh"))))))

(deftest xpath-results-with-predicates-test
  (let [pipeline (compile-xpath "./outE[@label = 'knows']/inV[@name != 'vadas']/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= (.size results) 1))
      (is (= results '("josh"))))))

