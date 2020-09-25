(ns slf4j-timbre.factory
	(:gen-class
		:name com.github.fzakaria.slf4j.timbre.TimbreLoggerFactory
		:implements [org.slf4j.ILoggerFactory]
		:state state
		:init init)
	(:require slf4j-timbre.adapter)
	(:import com.github.fzakaria.slf4j.timbre.TimbreLoggerAdapter))

(defn -init
	[]
	[[] (atom {})])

(defn -getLogger
  [this logger-name]
  (let [loggers (.state this) loggers-map @loggers preload (System/getProperty "timbre.preload")]
    (when preload
      (#'clojure.core/serialized-require (symbol preload)))
    (if-let [existing (get loggers-map logger-name)]
      existing
      (let [loggers-map (swap! loggers
                               update
                               logger-name
                               (fn [v] (or v (TimbreLoggerAdapter. logger-name))))]
        (get loggers-map logger-name)))))
