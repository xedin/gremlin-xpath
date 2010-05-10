(ns gremlin.contrib.xpath.parser)

(defn tokens
  "get tokens from current xpath line"
  [#^String line]
  (loop [chars (seq line)]
    (if (empty? chars)
      nil
      (let [current-char (first chars)
          is-quote? (or (= current-char \") (= current-char \'))
          is-sq-bracket? (or (= current-char \[) (= current-char \]))]
        (recur (rest chars))))))
