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

(defmulti step-token class)
(defmulti analize-path class)
(defmulti pipe-for-predicate class)

(defmethod pipe-for-predicate CoreOperationEqual
  [operation]
  (seq operation))

(defn- get-axis [step]
  (->> step .getAxis (Step/axisToString)))

(defmulti pipe-for-step get-axis)

(defmethod pipe-for-step "self" [_]
  (IdentityPipe.))

(defmethod pipe-for-step "child" [step]
  (let [token (step-token (.getNodeTest step))]
    (cond (= token "inV")  (EdgeVertexPipe. IN_VERTEX)
          (= token "inE")  (VertexEdgePipe. IN_EDGES)
          (= token "outV") (EdgeVertexPipe. OUT_VERTEX)
          (= token "outE") (VertexEdgePipe. OUT_EDGES)
          :else (throw (Exception. (str "Could not map 'child::" token "' to any of existing pipes."))))))

(defmethod pipe-for-step "attribute" [step]
  (PropertyPipe. (step-token (.getNodeTest step))))

(defn- create-pipe [step]
  [(pipe-for-step step) (map pipe-for-predicate (seq (.getPredicates step)))])

(defmethod analize-path LocationPath
  [expr]
  (map #(create-pipe %) (seq (.getSteps expr))))

(defmethod analize-path ExpressionPath
  [expr]
  (println "expr path"))

(defmethod analize-path ExtensionFunction
  [expr]
  (println "ext func"))

(defmethod step-token NodeNameTest [node]
  (.. node getNodeName getName))

(defmethod step-token NodeTypeTest [node]
  (->> node .getNodeType (NodeTypeTest/nodeTypeToString )))

(defmethod step-token ProcessingInstructionTest [node]
  ())

(defonce graph (TinkerGraphFactory/createTinkerGraph))
(defonce vertex (.getVertex graph 1))

(defn parse [#^String xpath]
  (let [compiler (TreeCompiler.)
        path (Parser/parseExpression xpath compiler)
        pipeline (analize-path path)] pipeline))

