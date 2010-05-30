(ns gremlin.contrib.xpath.enums
  #^{:author "Pavel A. Yaskevich"
     :doc "Pipes Enums."}
  (:import [gremlin.contrib.xpath.helpers PipesEnumHelper]))

;; gremlin pipes pgm enums
(defonce IN_VERTEX  (PipesEnumHelper/IN_VERTEX))
(defonce OUT_VERTEX (PipesEnumHelper/OUT_VERTEX))
(defonce IN_EDGES  (PipesEnumHelper/IN_EDGES))
(defonce OUT_EDGES (PipesEnumHelper/OUT_EDGES))
(defonce EQUALS (PipesEnumHelper/EQUALS))
(defonce NOT_EQUALS (PipesEnumHelper/NOT_EQUALS))
(defonce GREATER_THAN_EQUAL (PipesEnumHelper/GREATER_THAN_EQUAL))
(defonce GREATER_THAN (PipesEnumHelper/GREATER_THAN))
(defonce LESS_THAN (PipesEnumHelper/LESS_THAN))
(defonce LESS_THAN_EQUAL (PipesEnumHelper/LESS_THAN_EQUAL))
