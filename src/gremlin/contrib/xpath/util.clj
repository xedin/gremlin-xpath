(ns gremlin.contrib.xpath.util
  (:import [org.apache.commons.jxpath.ri InfoSetUtil]
           [org.apache.commons.jxpath.ri.compiler Step]))

(defn get-axis [step]
  (->> step .getAxis (Step/axisToString)))

;; TODO: skip only first and last \' with regex
(defn skip-quotes [#^String line]
  (apply str (remove #(= \' %) (seq line))))

(defn skip-spaces [#^String line]
  (apply str (remove #(= \space %) (seq line))))

(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn xpath-constant-value [constant]
  (let [str-value (str constant)
        num-value (InfoSetUtil/doubleValue str-value)]
    (if (= (.isNaN num-value) true)
      (skip-quotes str-value)
      num-value)))

