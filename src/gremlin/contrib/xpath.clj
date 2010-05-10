(ns gremlin.contrib.xpath
  (:use [gremlin.contrib.xpath util parser]))

(defn- parse-xpath-string [#^String xpath-line] ())

(defn compile-xpath [#^String xpath-line] ())
(comment
  (ns gremlin.contrib.xpath)
  (load "xpath")
  (load-xml-document "examples/bookstore.xml")
  (compile-xpath "/book/title[@lang = eng]")
)
