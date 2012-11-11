(ns fn.trace
  (:require [clojure.pprint :as pp]))

(def ^{:dynamic true
       :doc "Current stack depth of traced function calls."}
 *trace-depth* 0)

(defmethod print-method Throwable [t out]
  (print-ctor t (fn [o w]
                  (print-method (.getMessage t) w)) out))

(defn trace-indent
  "Returns an indentation string based on *trace-depth*"
  []
  (apply str (take *trace-depth* (repeat "|    "))))

(defn text-format [name value & [out?]]
  (let [label (when name (format "%6s: " name))]
    (if out?
      (str label (trace-indent) "=> " (pr-str value))
      (str label (trace-indent) (pr-str value)))))

(declare take-while-realized)

(defprotocol Realized
  (realized [coll])
  (realized-part [x]))

(extend-protocol Realized
  clojure.lang.IPending
  (realized [coll]
    (realized? coll))
  (realized-part [coll]
    (take-while-realized coll))

  clojure.lang.ASeq
  (realized [coll] true)
  (realized-part [coll]
    (take-while-realized coll))
  
  java.lang.Object
  (realized [o] true)
  (realized-part [o] o)

  nil
  (realized [n] true)
  (realized-part [n] nil))

(defn take-while-realized
  "Returns a lazy sequence of successive items from coll while
  (realized coll) returns true."
  [s]
  (lazy-seq
    (when (and s (realized s))
      (cons (first s) (take-while-realized (rest s))))))

(defn ^:dynamic tracer
  "This function is called by trace.  Prints to standard output, but
  may be rebound to do anything you like.  'name' is optional."
  [name value & [out?]]
  (println (text-format name value)))

(def thread-local-filewriter
  (proxy [ThreadLocal] []
    (initialValue [] (->> (Thread/currentThread)
                        .getName
                        (format "%s.trace")
                        java.io.FileWriter.))))

(defn thread-tracer
  "Writes trace to a file based on the current thread name"
  [name value & [out?]]
  (binding [*out* (.get thread-local-filewriter)]
    (println (text-format name value out?))))

(defn per-thread-tracer
  "Returns a tracer that writes to a file based on the name of the
   current thread. Warning: binding tracer to functions produced here
   might not have the effect you want. Futures, for instance, will use
   the same binding as the parent thread, and cause shuffled tracing
   entries in the output."
  [& [formatter]]
  (let [formatter (or formatter text-format)
        tracefile-name (str (.getName (Thread/currentThread)) ".trace")
        tr-file-writer (java.io.FileWriter. tracefile-name)]
    (fn [name value & [out?]]
      (binding [*out* tr-file-writer]
        (println (formatter name value out?))))))

(defn trace-fn-call
  "Traces a single call to a function f with args.  'name' is the
  symbol name of the function."
  [name f args]
  (let [id (gensym "t")]
    (tracer id (cons name (map realized-part args)))
    (let [[value err] (binding [*trace-depth* (inc *trace-depth*)]
                        (try [(apply f args) nil]
                             (catch Throwable e [e e])))]
      (binding [*print-length* (or *print-length* 10)
                *print-level* (or *print-level* 10)] ;;catch-all max, rebind if you want more/less
        (tracer id (realized-part value) true))
      (when err (throw err))
      value)))



(defmacro deftrace
  "Use in place of defn; traces each call/return of this fn, including
  arguments.  Nested calls to deftrace'd functions will print a
  tree-like structure."
  [name & definition]
  `(do
     (def ~name)
     (let [f# (fn ~@definition)]
       (defn ~name [& args#]
         (trace-fn-call '~name f# args#)))))

(defn rebind-map [fnames]
  (into {}
        (for [fname fnames :let [thisvar (resolve fname)] :when thisvar]
          (let [fn-to-trace (var-get thisvar)]
            [thisvar (fn [& args]
                       (trace-fn-call fname fn-to-trace  args))]))))

(defmacro dotrace
  "Given a sequence of function identifiers, evaluate
   the body expressions in an environment in which the identifiers are
   bound to the traced functions. Does not work on inlined functions,
   such as clojure.core/+"
  [fnames & exprs]
  `(with-redefs-fn (rebind-map ~fnames) (fn [] ~@exprs)) )

(defn non-macro-fn? [v]
  (and (fn? (deref v)) (not (:macro (meta v)))))

(defn all-fn-in-ns [ & namespaces]
  (for [namespace namespaces
        [k v] (ns-interns namespace)
        :when (non-macro-fn? v)]
    (symbol (str (.ns v) "/" (.sym v)))))

(defn all-fns
  "Takes a list of symbols corresponding to either fns or namespaces,
   namespaces are expanded to all the fns in that namespace. Returns
   the larger list of symbols."
  [syms]
  (mapcat (fn [sym]
            (cond (find-ns sym) (all-fn-in-ns sym)
                  (try (resolve sym) (catch Exception _ nil)) (list sym)
                  :else (list)))
          syms))

(defmacro dotrace-all [syms & forms]
  `(dotrace
       (all-fns ~syms) ~@forms))

(defn log-dispatch [obj]
  (if (-> obj meta :log)
    (binding [*print-level* 10
              *print-length* 100]
      (let [indent 2]
       (doseq [[o out?] obj]
         (if-not out?
           (do
             (pp/pprint-indent :current indent)
             (pp/code-dispatch o))
           (do
             (pp/pprint-indent :current (- indent))
             (pp/simple-dispatch o)))
         (pp/pprint-newline :mandatory)))) 
    
    (pp/code-dispatch obj)))
