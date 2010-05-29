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

(deftest and-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE/inV[@name != 'vadas' and @name != 'lop']/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= (.size results) 1))
      (is (= results '("josh")))))
  (let [pipeline (compile-xpath "./outE[@label != 'created' and @label = 'knows']/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= (.size results) 2))
      (is (= results '("vadas" "josh"))))))

(deftest or-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@label = 'created' or @weight >= 0.5]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("vadas" "lop" "josh"))))))

(deftest advanced-or-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@label='created' or (@label='knows' and @weight <= 0.5)]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("vadas" "lop"))))))

(deftest greater-than-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@weight > 0.5]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("josh"))))))

(deftest less-than-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@weight < 0.5]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("lop"))))))

(deftest greater-than-or-equal-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@weight >= 0.5]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("vadas" "josh"))))))

(deftest less-than-or-equal-operation-in-predicates-test
  (let [pipeline (compile-xpath "./outE[@weight <= 0.5]/inV/@name")]
    (.setStarts pipeline (.iterator (list vertex)))
    (let [results (seq pipeline)]
      (is (= results '("vadas" "lop"))))))
