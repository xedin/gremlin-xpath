(ns gremlin.contrib.xpath.parser
  (:use [gremlin.contrib.xpath.util])
  (:import [com.tinkerpop.pipes.serial IdentityPipe Pipeline]
           [org.apache.commons.jxpath.ri Parser QName]
           [com.tinkerpop.pipes.serial.pgm EdgeVertexPipe VertexEdgePipe LabelFilterPipe PropertyFilterPipe PropertyPipe]
           [org.apache.commons.jxpath.ri.compiler TreeCompiler LocationPath ExpressionPath ExtensionFunction Step NodeNameTest  NodeTypeTest ProcessingInstructionTest CoreOperationEqual]
           [gremlin.contrib.xpath.helpers PipesEnumHelper]
           [com.tinkerpop.blueprints.pgm.impls.tg TinkerGraphFactory]))

;; declaring gremlin pipes pgm enums
(defonce IN_VERTEX  (PipesEnumHelper/IN_VERTEX))
(defonce OUT_VERTEX (PipesEnumHelper/OUT_VERTEX))

(defonce IN_EDGES  (PipesEnumHelper/IN_EDGES))
(defonce OUT_EDGES (PipesEnumHelper/OUT_EDGES))

(defmulti analize-step class)
(defmulti analize-path class)
(defmulti pipe-for-predicate class)

(defmethod pipe-for-predicate CoreOperationEqual
  [operation]
  (seq (.getArguments operation)))

(defn- create-pipe [step]
  (let [node-test (.getNodeTest step)
        token (analize-step node-test)
        step-axis (->> step .getAxis (Step/axisToString))
        predicates (.getPredicates step)]
    [(cond (= token "node") (IdentityPipe.)
           (= token "inV")  (EdgeVertexPipe. IN_VERTEX)
           (= token "inE")  (VertexEdgePipe. IN_EDGES)
           (= token "outE") (VertexEdgePipe. OUT_EDGES)
           (= token "outV") (EdgeVertexPipe. OUT_VERTEX)
           :else (if (= step-axis "attribute")
                   (PropertyPipe. token)
                   (throw (Exception. (str "Could not map '" step-axis "::" token "' to any of existing pipes.")))))
     (if-not (empty? predicates)
       (map pipe-for-predicate (seq predicates)) '())]))

(defmethod analize-path LocationPath
  [expr]
  (map #(create-pipe %) (seq (.getSteps expr))))

(defmethod analize-path ExpressionPath
  [expr]
  (println "expr path"))

(defmethod analize-path ExtensionFunction
  [expr]
  (println "ext func"))

(defmethod analize-step NodeNameTest [node]
  (.. node getNodeName getName))

(defmethod analize-step NodeTypeTest [node]
  (->> node .getNodeType (NodeTypeTest/nodeTypeToString )))

(defmethod analize-step ProcessingInstructionTest [node]
  ())

(defonce graph (TinkerGraphFactory/createTinkerGraph))
(defonce vertex (.getVertex graph 1))

(defn parse [#^String xpath]
  (let [compiler (TreeCompiler.)
        path (Parser/parseExpression xpath compiler)
        pipeline (analize-path path)]
    pipeline))

