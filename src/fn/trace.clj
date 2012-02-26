(ns fn.trace
  (:require [clojure.prxml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]))

(def ^{:dynamic true
       :doc "Current stack depth of traced function calls."}
 *trace-depth* 0)

(defn trace-indent
  "Returns an indentation string based on *trace-depth*"
  []
  (apply str (take *trace-depth* (repeat "|    "))))

(defn clj-format [name value & [out?]]
  (let [[printer miser margin] (if out?
                                 [pp/simple-dispatch 120 150]
                                 [pp/code-dispatch 90 110])
        val (binding [pp/*print-miser-width* miser
                      pp/*print-right-margin* margin]
              (with-out-str (pp/with-pprint-dispatch printer
                              (pp/pprint value))))]
    (pr-str [*trace-depth* name val out?])))

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
    (.sym v)))

(defn all-fns
  "Takes a list of symbols corresponding to either fns or namespaces,
   namespaces are expanded to all the fns in that namespace. Returns
   the larger list of symbols."
  [ & syms]
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
       (apply all-fns ~syms) ~@forms))

(defn to-xml-tree
  "takes a filename that was created with clj-format, reads it and turns it into data that can be passed to prxml for html output."
  [f]
  (let [rdr (java.io.BufferedReader. 
             (java.io.FileReader. f))
        cdata (fn [form] [:script {:type "syntaxhighlighter" :class "brush: clj"}
                         [:cdata! (str form)]])
        data (loop [lines (line-seq rdr) acc [] ins-point [0]]
               (comment (clojure.pprint/pprint [ acc ins-point]))
               (if (empty? lines) acc
                   (let [[d _ form out?] (-> lines first read-string)
                         v (if out? (cdata form)
                               [:div {:class "trace"} 
                                (cdata form)])]
                     (recur (drop 1 lines)
                            (assoc-in acc ins-point v)
                            (if out?
                              (let [popped (pop ins-point)]
                                (conj (vec (butlast popped)) (inc (last popped))))
                              (conj ins-point 3))))))]
    (.close rdr)
    data))

(defn to-html "Takes a trace file produced with clj-format, turns it into html with syntax highlighting and css."
  [f dest-dir & [sh-url]]

  (binding [*out* (java.io.FileWriter. (str dest-dir "/" (-> f java.io.File. .getName) ".html"))
            xml/*prxml-indent* 4
            xml/*html-compatible* true]
    (xml/prxml [:doctype! "html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
                "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""]
               [:html {:xmlns "http://www.w3.org/1999/xhtml"}
                [:head
                 [:meta  {:http-equiv "Content-Type"
                          :content "text/html;charset=utf-8"}]
                 [:title f]
                 [:script {:type "text/javascript" :src (str sh-url "scripts/shCore.js")} " "]
                 [:script {:type "text/javascript" :src (str sh-url "scripts/shBrushClojure.js")} " "]
                 [:link {:type "text/css" :rel "stylesheet" :href "styles/shTrace.css"}]
                 [:script {:type "text/javascript"}
                  [:raw! "SyntaxHighlighter.defaults['gutter'] = false;
                          SyntaxHighlighter.defaults['toolbar'] = false;
                          SyntaxHighlighter.all();"]]]
                [:body [:h1 "Trace log"]
                 (vec (concat [:div {:class "highlight"}] (to-xml-tree f)))]])
    (.close *out*)))

(defn htmlify [dest-dir trace-files sh-url]
  (let [style "styles/shTrace.css"
        sfile (java.io.File. (str dest-dir "/" style))]
    (.mkdirs (.getParentFile sfile))
    (io/copy (-> (ClassLoader/getSystemClassLoader) (.getResourceAsStream style))
              sfile))
  (doseq [file trace-files]
    (to-html file dest-dir sh-url)))

(defn log-dispatch [obj]
  (if (-> obj meta :log)
    (let [indent 2]
      (doseq [[o out?] obj]
        (if-not out?
          (do
            (pp/pprint-indent :current indent)
            (pp/code-dispatch o))
          (do
            (pp/pprint-indent :current (- indent))
            (pp/simple-dispatch o)))
        (pp/pprint-newline :mandatory))) 
    
    (pp/code-dispatch obj)))
