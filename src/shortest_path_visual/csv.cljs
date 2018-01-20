(ns shortest-path-visual.csv
  (:require
    [instaparse.core :as insta]
    [clojure.string :as string]))

(def csv-gramma
  "csv : [row] {<eol> row}
row : field {<','> field}
<field> : [<ws>] id [<ws>]
ws : #'\\s*'
eol : '\n' | '\r\n' | '\n\r'
<id> : literal | numeral | quoted | html
<literal> : #'[a-zA-Z\\200-\\377][a-zA-Z\\200-\\377\\_0-9 ]*'
<numeral> : #'[-]?(.[0-9]+|[0-9]+(.[0-9]*)?)'
<quoted> : <'\"'> #'(?:[^\"\\\\]|\\\\.)*' <'\"'>
<html> : #'<[^>]*>'")

(def parse-csv
  (insta/parser csv-gramma))

(defn ^:private reverse-merge [& maps]
  (apply merge (reverse maps)))

(defn collect-row [g [_ from & tos]]
  (-> g
      (update-in [:nodes] reverse-merge (zipmap tos (repeat {})))
      (assoc-in [:nodes from] {})
      (assoc-in [:edges from] (zipmap tos (repeat {})))))

(defn read-graph [csv]
  (let [ast (parse-csv csv)]
    (if (insta/failure? ast)
      (js/alert (str "Failed to parse CSV: " ast))
      (reduce collect-row {} (nnext ast)))))
