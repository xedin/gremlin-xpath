(ns clojure.contrib.xpath.util)

(defstruct token-attr :name :value)

(defn node? [element]
  (if (nil? (:tag element)) false true))

(defn- names-match? [node-name token-name]
  (= node-name token-name))

(defn- attrs-match? [node-attrs token-attrs]
  (loop [all-match true attrs token-attrs]
    (if (or (empty? attrs) (false? all-match))
      all-match
      (let [current-attr (first attrs)
            corr-node-value ((:name current-attr) node-attrs)]
        (if (nil? corr-node-value)
          (recur false (rest attrs))
          (recur (= corr-node-value (:value current-attr)) (rest attrs)))))))

(defn match? [node token]
  (if (and (names-match? (:tag node) (:name token))
           (attrs-match? (:attrs node) (:attrs token))) true false))

(defn flatten [x]
  (let [s? #(instance? clojure.lang.Sequential %)]
    (filter (complement s?) (tree-seq s? seq x))))

(defn xpath-token-name [token]
  (loop [token-name () chars (seq token)]
    (if (or (empty? chars) (= (first chars) \[))
      (keyword (apply str (reverse token-name)))
      (recur (conj token-name (first chars)) (rest chars)))))

(defn- parse-xpath-attr [attr]
  (apply (fn [name value] (struct token-attr (keyword (apply str (rest name))) value))  (seq (.split attr " = "))))

(defn xpath-token-attrs [token]
  (map parse-xpath-attr (map #(last %) (re-seq #"\[(.+?)\]" token))))
