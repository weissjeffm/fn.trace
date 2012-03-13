(ns fn.trace
  (:require [clojure.prxml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

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

(defn ^:dynamic tracer
  "This function is called by trace.  Prints to standard output, but
  may be rebound to do anything you like.  'name' is optional."
  [name value & [out?]]
  (println (text-format name value)))

(defn per-thread-tracer [& [formatter]]
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
    (tracer id (cons name args))
    (let [[value err] (binding [*trace-depth* (inc *trace-depth*)]
                        (try [(apply f args) nil]
                             (catch Throwable e [e e])))]
      (binding [*print-length* (or *print-length* 10)
                *print-level* (or *print-level* 10)] ;;catch-all max, rebind if you want more/less
        (tracer id value true))
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
  (mapcat (fn [sym] (if (find-ns sym) (all-fn-in-ns sym)
                       (list sym))) syms))
(defn as-trace-list
  "Given a map, creates a list of functions to trace. Any function in a namespace
  in :namespaces will be traced, plus any function listed in :fns. Any
  function listed in :exclude will not be traced."
  [{:keys [namespaces fns exclude]}]
  (vec (remove (set exclude)
               (concat (mapcat all-fn-in-ns namespaces) fns))))

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
