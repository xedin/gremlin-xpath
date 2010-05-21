(ns gremlin.contrib.xpath.util)

(defn skip-spaces [#^String line]
  (remove #(= \space %) (seq line)))

(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

