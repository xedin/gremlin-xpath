(ns gremlin.contrib.xpath.compiler
  (:use [gremlin.contrib.xpath.util]
        [gremlin.contrib.xpath.enums])
  (:import [com.tinkerpop.pipes.serial IdentityPipe Pipeline]
           [org.apache.commons.jxpath.ri Parser QName]
           [com.tinkerpop.pipes.serial.filter AndFilterPipe OrFilterPipe]
           [com.tinkerpop.pipes.serial.pgm EdgeVertexPipe VertexEdgePipe LabelFilterPipe PropertyFilterPipe PropertyPipe]
           [org.apache.commons.jxpath.ri.compiler Path LocationPath ExpressionPath]
           [org.apache.commons.jxpath.ri.compiler TreeCompiler Expression Operation VariableReference Constant]
           [org.apache.commons.jxpath.ri.compiler CoreOperationEqual CoreOperationNotEqual CoreOperationAnd CoreOperationOr]
           [org.apache.commons.jxpath.ri.compiler CoreOperationGreaterThan CoreOperationLessThan CoreOperationGreaterThanOrEqual CoreOperationLessThanOrEqual]
           [org.apache.commons.jxpath.ri.compiler ExtensionFunction Step NodeNameTest  NodeTypeTest ProcessingInstructionTest]))

(defmulti step-token class)
(defmulti analize-path class)

(defmulti pipe-for-step get-axis)
(defmulti pipe-for-predicate class)

(defn- expr-type [expr]
  (let [expr-class (.getClass expr)
        superclass (.getSuperclass expr-class)]
    (cond (= superclass Path) :path
          (= superclass Operation) :operation
          (= superclass Expression)
            (cond (= expr-class Constant) :constant
                  (= expr-class VariableReference) :variable)
          :else (throw (Exception. "Could not determine expression type.")))))

(defn- pipe-for-logic-operation [arguments filter]
  (let [operand-one (first arguments)
        operand-two (last arguments)
        operand-one-type (expr-type operand-one)
        operand-two-type (expr-type operand-two)]
    (cond (and (= operand-one-type :path) (= operand-two-type :constant))
            (let [operand-one-step  (first (.getSteps operand-one))
                  operand-one-axis  (get-axis operand-one-step)
                  operand-one-token (step-token (.getNodeTest operand-one-step))
                  operand-two-value (xpath-constant-value operand-two)]
              (if (= operand-one-axis "attribute")
                (if (= operand-one-token "label")
                  (LabelFilterPipe. operand-two-value filter)
                  (PropertyFilterPipe. operand-one-token operand-two-value filter))))
          :else (throw (Exception. "not implemented yet.")))))

;; EQUAL, NOT_EQUAL
(defmethod pipe-for-predicate CoreOperationEqual [operation]
  (pipe-for-logic-operation (.getArguments operation) EQUALS))

(defmethod pipe-for-predicate CoreOperationNotEqual [operation]
  (pipe-for-logic-operation (.getArguments operation) NOT_EQUALS))

;;  GREATER_THAN, GREATER_THAN_EQUAL, LESS_THAN, LESS_THEN_EQUAL
(defmethod pipe-for-predicate CoreOperationGreaterThan [operation]
  (pipe-for-logic-operation (.getArguments operation) GREATER_THAN))

(defmethod pipe-for-predicate CoreOperationLessThan [operation]
  (pipe-for-logic-operation (.getArguments operation) LESS_THAN))

(defmethod pipe-for-predicate CoreOperationGreaterThanOrEqual [operation]
  (pipe-for-logic-operation (.getArguments operation) GREATER_THAN_EQUAL))

(defmethod pipe-for-predicate CoreOperationLessThanOrEqual [operation]
  (pipe-for-logic-operation (.getArguments operation) LESS_THAN_EQUAL))

;; AND & OR operations
(defmethod pipe-for-predicate CoreOperationAnd [operation]
  (AndFilterPipe. (map pipe-for-predicate (.getArguments operation))))

(defmethod pipe-for-predicate CoreOperationOr [operation]
  (OrFilterPipe. (map pipe-for-predicate (.getArguments operation))))

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

(defmethod analize-path LocationPath [expr]
  (map #(create-pipe %) (seq (.getSteps expr))))

(defmethod analize-path ExpressionPath [expr]
  (println "expr path"))

(defmethod analize-path ExtensionFunction [expr]
  (println "ext func"))

(defmethod step-token NodeNameTest [node]
  (.. node getNodeName getName))

(defmethod step-token NodeTypeTest [node]
  (->> node .getNodeType (NodeTypeTest/nodeTypeToString)))

(defmethod step-token ProcessingInstructionTest [node]
  '())

(defn compile-xpath [#^String xpath]
  (let [compiler (TreeCompiler.)
        path (Parser/parseExpression xpath compiler)
        pipes (flatten (analize-path path))]
    (Pipeline. pipes)))
