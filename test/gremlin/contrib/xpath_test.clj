(ns gremlin.contrib.xpath-test
  (:use [clojure.test])
  (:import [gremlin.contrib.xpath XPath]
           [com.tinkerpop.pipes.serial Pipeline]))

(deftest jv-compile-method-test
  (let [xpath (XPath.)
        pipeline (.compile xpath "./outE/inV/@name")]
    (is (= (.getClass pipeline) Pipeline))))
