(defproject gremlin-xpath "0.1"
  :description "Compiler from XPath to Gremlin Pipes."
  
  :dependencies [[org.clojure/clojure "1.1.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]
                 [commons-jxpath/commons-jxpath "1.3"]
                 [com.tinkerpop/pipes "0.1-SNAPSHOT"]]
    
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]]
    
  :repositories {"maven-repository" "http://mvnrepository.com"
                 "tinkerpop-repository" "http://tinkerpop.com/maven2"}
  
  :source-path "src")

