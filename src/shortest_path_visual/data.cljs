(ns shortest-path-visual.data
  (:require
    [clojure.string :as string]
    [shortest-path-visual.csv :as csv]
    [shortest-path-visual.dot :as dot]))

(def tiny
  {:nodes
   {"0" {:node/name "A", :db/id "0"},
    "1" {:node/name "B", :db/id "1"},
    "2" {:node/name "C", :db/id "2"},
    "3" {:node/name "D", :db/id "3"},
    "4" {:node/name "E", :db/id "4"},
    "5" {:node/name "F", :db/id "5"},
    "6" {:node/name "G", :db/id "6"},
    "7" {:node/name "H", :db/id "7"}},
   :edges
   {"4"
    {"5"
     {:edge/weight 0.35,
      :edge/distance 10.5,
      :edge/from "4",
      :edge/to "5",
      :db/id "4-5"},
     "7"
     {:edge/weight 0.37,
      :edge/distance 11.1,
      :edge/from "4",
      :edge/to "7",
      :db/id "4-7"}},
    "5"
    {"4"
     {:edge/weight 0.35,
      :edge/distance 10.5,
      :edge/from "5",
      :edge/to "4",
      :db/id "5-4"},
     "7"
     {:edge/weight 0.28,
      :edge/distance 8.4,
      :edge/from "5",
      :edge/to "7",
      :db/id "5-7"},
     "1"
     {:edge/weight 0.32,
      :edge/distance 9.6,
      :edge/from "5",
      :edge/to "1",
      :db/id "5-1"}},
    "7"
    {"5"
     {:edge/weight 0.28,
      :edge/distance 8.4,
      :edge/from "7",
      :edge/to "5",
      :db/id "7-5"},
     "3"
     {:edge/weight 0.39,
      :edge/distance 11.700000000000001,
      :edge/from "7",
      :edge/to "3",
      :db/id "7-3"}},
    "0"
    {"4"
     {:edge/weight 0.38,
      :edge/distance 11.4,
      :edge/from "0",
      :edge/to "4",
      :db/id "0-4"},
     "2"
     {:edge/weight 0.26,
      :edge/distance 7.800000000000001,
      :edge/from "0",
      :edge/to "2",
      :db/id "0-2"}},
    "1"
    {"3"
     {:edge/weight 0.29,
      :edge/distance 8.7,
      :edge/from "1",
      :edge/to "3",
      :db/id "1-3"}},
    "2"
    {"7"
     {:edge/weight 0.34,
      :edge/distance 10.200000000000001,
      :edge/from "2",
      :edge/to "7",
      :db/id "2-7"}},
    "6"
    {"2"
     {:edge/weight 0.40,
      :edge/distance 12,
      :edge/from "6",
      :edge/to "2",
      :db/id "6-2"},
     "0"
     {:edge/weight 0.58,
      :edge/distance 17.4,
      :edge/from "6",
      :edge/to "0",
      :db/id "6-0"},
     "4"
     {:edge/weight 0.93,
      :edge/distance 27.900000000000002,
      :edge/from "6",
      :edge/to "4",
      :db/id "6-4"}},
    "3"
    {"6"
     {:edge/weight 0.52,
      :edge/distance 15.600000000000001,
      :edge/from "3",
      :edge/to "6",
      :db/id "3-6"}}}})

(defn ends-with [s suffix]
  (not (neg? (.indexOf s suffix (- (.-length s) (.-length suffix))))))

(defn read-file [r file deserialize]
  (if js/FileReader
    (let [reader (js/FileReader.)]
      (set! (.-onload reader)
            (fn file-loaded [e]
              (when-let [new-graph (deserialize (.. e -target -result))]
                (reset! r new-graph))))
      (.readAsText reader file))
    (js/alert "Browser does not support FileReader")))

(defn collect-edge [g [from to weight]]
  (assoc-in g [:edges from to] {:edge/weight weight
                                :edge/distance (* weight 30)
                                :edge/from from
                                :edge/to to
                                :db/id (str from "-" to)}))

(defn read-weighted-graph [s]
  (let [[node-count edge-count & edges] (string/split-lines s)]
    (reduce
      collect-edge
      {:nodes (zipmap (map str (range node-count))
                      (for [n (map str (range node-count))]
                        {:node/name n
                         :db/id n}))}
      (map #(string/split % #" ") edges))))

(defn import-graph [e g]
  (when-let [file (aget e "target" "files" 0)]
    (if-let [r (cond (ends-with (.-name file) ".txt") csv/read-graph
                     (ends-with (.-name file) ".dot") dot/read-graph)]
      (read-file g file r)
      (js/alert "Must supply a .dot or .txt file"))))
