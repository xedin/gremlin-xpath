(ns clojure.contrib.xpath
  (:use [clojure.contrib.lazy-xml :as xml]
        [clojure.contrib.xpath.util])
  (:import [java.io File FileReader]))

(defonce *root* (atom nil))
(defstruct token :name :attrs)

(defn load-xml-document [#^String filename]
  (with-open [xml-file-reader (FileReader. (File. filename))]
    (reset! *root* (xml/parse-trim xml-file-reader))))

(defn- parse-xpath-string [#^String xpath-line]
  (map #(struct token (xpath-token-name %) (xpath-token-attrs %))
       (remove empty? (seq (.split xpath-line "/")))))

(defn- nodes-by-token [coll token]
  (loop [result [] nodes coll]
    (if (empty? nodes)
      result
      (let [current-node (first nodes)]
        (if (match? current-node token)
          (recur (conj result current-node) (rest nodes))
          (recur (conj result (nodes-by-token (:content current-node) token)) (rest nodes)))))))

(defn- filter-by-token [coll token]
    (flatten (nodes-by-token coll token)))

(defn compile-xpath [#^String xpath-line]
  (let [xpath-tokens (parse-xpath-string xpath-line)]
    (loop [result (:content @*root*) tokens xpath-tokens]
      (if (or (empty? tokens) (empty? result))
        result
        (recur (filter-by-token result (first tokens)) (rest tokens))))))

(comment
  (ns clojure.contrib.xpath)
  (load "xpath")
  (load-xml-document "examples/bookstore.xml")
  (compile-xpath "/book/title[@lang = eng]")
)
