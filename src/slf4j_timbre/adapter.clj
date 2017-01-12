(ns slf4j-timbre.adapter
	(:gen-class
		:name         com.github.fzakaria.slf4j.timbre.TimbreLoggerAdapter
		:implements   [org.slf4j.Logger]
		:state        state
		:init         init
		:constructors {[String] []})
	(:require
		[taoensso.timbre :as timbre])
	(:import
		[org.slf4j.helpers FormattingTuple MessageFormatter]
		org.slf4j.Marker))

(defn -init
	[logger-name]
	[[] logger-name])

(defn -getName
	[this]
	(.state this))

(defmacro define-methods
	"Defines the various overloads for a given logging method (e.g., -info).
	Several have the same arity so we use an undocumented Clojure feature [1] to specify their type signatures.
	This macro expands into a (do ...) sexpr containing a defn for each of the ten variants declared in the Logger interface.
	[1] https://groups.google.com/d/embed/msg/clojure/KmNbLo8xTSs/d1Rs3Cs6DbAJ"
	[method-name level]
	`(do
		~@(for [signature    ["-String" "-String-Object" "-String-Object-Object" "-String-Object<>" "-String-Throwable"]
		        with-marker? [false true]]
			(let [func-sym   (symbol (str method-name (when with-marker? "-Marker") signature))
			      args-sym   (gensym "args")
			      ns-str-sym (gensym "ns-str")
			      file-sym   (gensym "file")
			      line-sym   (gensym "line")]

				`(defn ~func-sym [this# & ~args-sym]
					(let [~ns-str-sym (.getName this#)]
						(when (timbre/log? ~level ~ns-str-sym)
							(let [context#    ~(when with-marker? `(when-let [marker# (first ~args-sym)] {:marker (.getName marker#)}))
							      ; we do a nil check above because log4j-over-slf4j passes a null Marker instead of calling the correct (Marker-free) method
							      ~args-sym   ~(if with-marker? `(rest ~args-sym) args-sym)
							      stack#      (.getStackTrace (Thread/currentThread))
							      caller#     (second (drop-while #(not= (.getName (.getClass this#)) (.getClassName %)) stack#))
							      ~file-sym   (.getFileName caller#)
							      ~line-sym   (.getLineNumber caller#)]
								(timbre/with-context context#
									~(case signature
										"-String"
										`(let [[msg#] ~args-sym]
											(timbre/log! ~level :p [msg#] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym}))

										"-String-Object"
										`(let [[fmt# o#] ~args-sym
										       ft# (MessageFormatter/format fmt# o#)]
											(if-let [t# (.getThrowable ft#)]
												(timbre/log! ~level :p [t# (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})
												(timbre/log! ~level :p [   (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})))

										"-String-Object-Object"
										`(let [[fmt# o1# o2#] ~args-sym
										       ft# (MessageFormatter/format fmt# o1# o2#)]
											(if-let [t# (.getThrowable ft#)]
												(timbre/log! ~level :p [t# (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})
												(timbre/log! ~level :p [   (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})))

										"-String-Object<>"
										`(let [[fmt# os#] ~args-sym
										       ft# (MessageFormatter/arrayFormat fmt# os#)]
											(if-let [t# (.getThrowable ft#)]
												(timbre/log! ~level :p [t# (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})
												(timbre/log! ~level :p [   (.getMessage ft#)] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym})))

										"-String-Throwable"
										`(let [[msg# t#] ~args-sym]
											(timbre/log! ~level :p [t# msg#] {:?ns-str ~ns-str-sym :?file ~file-sym :?line ~line-sym}))))))))))))


(define-methods "-error" :error)
(define-methods "-warn"  :warn)
(define-methods "-info"  :info)
(define-methods "-debug" :debug)
(define-methods "-trace" :trace)

(defn -isErrorEnabled
	([this]   (timbre/log? :error (.getName this)))
	([this _] (timbre/log? :error (.getName this))))
(defn -isWarnEnabled
	([this]   (timbre/log? :warn (.getName this)))
	([this _] (timbre/log? :warn (.getName this))))
(defn -isInfoEnabled
	([this]   (timbre/log? :info (.getName this)))
	([this _] (timbre/log? :info (.getName this))))
(defn -isDebugEnabled
	([this]   (timbre/log? :debug (.getName this)))
	([this _] (timbre/log? :debug (.getName this))))
(defn -isTraceEnabled
	([this]   (timbre/log? :trace (.getName this)))
	([this _] (timbre/log? :trace (.getName this))))
