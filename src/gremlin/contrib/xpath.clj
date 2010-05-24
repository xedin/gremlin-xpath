(ns gremlin.contrib.xpath
  (:use [gremlin.contrib.xpath.compiler :as compiler])
  (:gen-class :name gremlin.contrib.xpath.XPath
              :methods [[compile [String] com.tinkerpop.pipes.serial.Pipeline]]))

(defn -compile [_ #^String line]
  (compiler/compile-xpath line))

