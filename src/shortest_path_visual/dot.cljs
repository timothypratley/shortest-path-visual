(ns shortest-path-visual.dot
  (:require
    [clojure.string :as string]
    [clojure.walk :as walk]
    [instaparse.core :as insta]))

(def dot-gramma
  "graph : <ws> [<'strict'> <ws>] ('graph' | 'digraph') <ws> [id <ws>] <'{'> stmt_list <'}'> <ws>
<stmt_list> : <ws> (stmt <ws> [<';'> <ws>])*
<stmt> : node | edge | attr | eq | subgraph
eq : id <ws> <'='> <ws> id
attr : ('graph' | 'node' | 'edge') <ws> attr_list
<attr_list> : (<'['> <ws> [a_list <ws>] <']'> <ws>)+
<a_list> : a (<(';' | ',')> <ws> a)*
<a> : id <ws> <'='> <ws> id <ws>
edge : (node_id | subgraph) <ws> edge_RHS [<ws> attr_list]
<edge_RHS> : (<edge_op> <ws> (node_id | subgraph) <ws>)+
edge_op : '->' | '--'
node : node_id [<ws> attr_list]
<node_id> : id [<ws> port]
port  : <':'> <ws> id [<ws> <':'> <ws> compass_pt] | <':'> <ws> compass_pt
compass_pt  : 'n' | 'ne' | 'e' | 'se' | 's' | 'sw' | 'w' | 'nw' | 'c' | '_'
subgraph  : ['subgraph' [<ws> id] <ws>] <'{'> <ws> stmt_list <ws> <'}'>
ws : #'\\s*'
<id> : literal | number | quoted | html
<literal> : #'[a-zA-Z\\200-\\377][a-zA-Z\\200-\\377\\_\\d]*'
<quoted> : <'\"'> #'(?:[^\"\\\\]|\\\\.)*' <'\"'>
<number> : #'-?([\\.]\\d+|\\d+(\\.\\d*)?)'
<html> : #'<[^>]*>'")

(def parse-dot
  (insta/parser dot-gramma))

(defn quote-escape [s]
  (str \" (string/replace s #"\"" "\\\"") \"))

(defn qualify-keywords [m q]
  (into {}
        (for [[k v] m]
          [(keyword q (name k)) v])))

(defn collect-statement [graph [statement-type & statement-body]]
  (condp = statement-type
    :node (let [[id & attrs] statement-body
                attr-map (qualify-keywords (apply hash-map attrs) "node")]
            (assoc-in graph [:nodes id] attr-map))
    :edge (let [[from to & attrs] statement-body
                attr-map (qualify-keywords (apply hash-map attrs) "edge")]
            (-> graph
                (assoc-in [:edges from to] (merge {:edge/from from :edge/to to} attr-map))
                (update-in [:nodes from] merge {})
                (update-in [:nodes to] merge {})))
    :attr (merge (qualify-keywords (apply hash-map statement-body) "graph") graph)
    :eq graph
    :subgraph graph
    graph))

(defn read-graph [s]
  (let [ast (parse-dot s)]
    (if (insta/failure? ast)
      (js/alert (str "Failed to parse dot file: " ast))
      (let [[_ _ & statements] ast
            [title] statements
            statements (if (string? title)
                         (rest statements)
                         statements)
            graph (if (string? title)
                    {:title title}
                    {})]
        (reduce collect-statement graph statements)))))
