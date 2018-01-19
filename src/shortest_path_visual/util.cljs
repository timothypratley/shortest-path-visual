(ns shortest-path-visual.util
  (:require
    [shortest-path-visual.d3 :as d3]
    [clojure.string :as string]
    [clojure.pprint :as pprint]
    [reagent.core :as reagent]
    [clojure.set :as set]))

(def tiny
  {:nodes
   {"0" {:node/name "0", :db/id "0"},
    "1" {:node/name "1", :db/id "1"},
    "2" {:node/name "2", :db/id "2"},
    "3" {:node/name "3", :db/id "3"},
    "4" {:node/name "4", :db/id "4"},
    "5" {:node/name "5", :db/id "5"},
    "6" {:node/name "6", :db/id "6"},
    "7" {:node/name "7", :db/id "7"}},
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

(defn import-button [label accept deserialize db]
  [:li
   [:a.btn.btn-file
    label
    [:input
     {:type "file"
      :name "import"
      :tab-index "-1"
      :accept accept
      :value ""
      :on-change
      (fn import-csv-change [e]
        (when-let [file (aget e "target" "files" 0)]
          (if (ends-with (.-name file) ".txt")
            (do (read-file db file deserialize))
            (js/alert "Must supply a .dot or .txt file"))))}]]])

(def node-types
  (reagent/atom {}))

(def edge-types
  (reagent/atom {}))

(def callbacks
  (reagent/atom {}))

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

(defn min-edge [candidates]
  (let [{:keys [edge/to edge/from distance]} (first (sort-by :distance (vals candidates)))]
    [to from distance]))

(defn add-candidates [g distance current-node visited candidates]
  (apply dissoc
         (merge candidates
                (for [[k v] (get-in @g [:edges current-node])]
                  [k (assoc
                       (select-keys v [:edge/weight :edge/from :edge/to :distance])
                       :distance (+ distance (:edge/weight v 1)))]))
         (keys visited)))

(def vis
  (reagent/atom nil))

(defn visualize-start [g start-node target-node]
  (reset! vis {:distance 0
               :status :searching
               :edge nil
               :visited {}
               :candidates {}})
  (swap! g assoc-in [:nodes start-node :node/color] "green")
  (swap! g assoc-in [:nodes target-node :node/color] "blue"))

(defn visualize-expand [g visited candidates]
  (swap! vis assoc
         :visited visited
         :candidates candidates)
  (swap! g update :nodes #(merge-with merge %1 %2)
         (zipmap (keys candidates)
                 (repeat {:node/color "cyan"})))
  (doseq [{:keys [edge/from edge/to]} (vals candidates)]
    (swap! g assoc-in [:edges from to :edge/color] "cyan")))

(defn visualize-visit [g edge visited candidates distance]
  (swap! vis assoc
         :edge edge
         :visited visited
         :candidates candidates
         :distance distance)
  (swap! g update :nodes #(merge-with merge %1 %2)
         (zipmap (keys visited)
                 (repeat {:node/color "yellow"}))))

(defn visualize-solution [g visited target-node distance]
  (swap! vis assoc
         :visited visited
         :distance distance
         :status :success)
  (swap! g assoc-in [:nodes target-node :node/color] "blue")
  (loop [n target-node]
    (when-let [v (visited n)]
      (swap! g assoc-in [:nodes v :node/color] "blue")
      (swap! g assoc-in [:edges v n :edge/color] "blue")
      (recur v))))

(defn visualize-fail [g]
  (swap! vis assoc
         :status :failed))

(defn slow-trampoline
  ([t f]
   (let [ret (f)]
     (if (fn? ret)
       (js/setTimeout #(slow-trampoline t ret) t)
       ret)))
  ([t f & args]
   (slow-trampoline t #(apply f args))))

(declare expand)

(defn visit [g target-node visited candidates]
  (let [[to from distance :as edge] (min-edge candidates)
        visited-result (conj visited edge)
        candidates-result (dissoc candidates to)]
    (visualize-visit g edge visited-result candidates-result distance)
    (cond
      (= to target-node) #(visualize-solution g visited-result target-node distance)
      (empty? candidates) #(visualize-fail g)
      :else #(expand g distance to target-node visited-result candidates-result))))

(defn expand [g distance current-node target-node visited candidates]
  (let [expanded-candidates (add-candidates g distance current-node visited candidates)]
    (visualize-expand g visited expanded-candidates)
    #(visit g target-node visited expanded-candidates)))

(defn shortest-path-step [g distance current-node target-node visited candidates]
  (let [candidates (add-candidates g distance current-node visited candidates)]
    (let [[to from distance-result :as me] (min-edge candidates)
          v (conj visited me)]
      (cond
        ;;TODO: unroll
        (= to target-node) v
        (empty? candidates) "FAIL"
        ;; timeout
        :else (recur g distance-result to target-node v candidates)))))

(defn shortest-path [g]
  ;;(prn "SP" (shortest-path-step g 0 "0" "5" {} {}))
  (visualize-start g "0" "5")
  (slow-trampoline 2000 expand g 0 "0" "5" {} {}))

(defn inspect [x]
  [:table
   {:style {:width "100%"
            :text-align "left"}}
   [:thead
    (into
      [:tr]
      (for [k (keys @vis)]
        [:th k]))]
   (into
     [:tr]
     (for [v (vals @vis)]
       [:td {:style {:vertical-align "top"
                     :width "20%"}}
        [:pre (with-out-str (pprint/pprint v))]]))])

;; TODO: edge/distance and distance is confusing

(defn graph-loader []
  (let [g (reagent/atom {})
        selected-id (reagent/atom nil)
        selected-edge-type (reagent/atom nil)]
    (fn a-graph-loader []
      [:div
       [:button
        {:on-click
         (fn [e]
           (reset! g tiny))}
        "Example"]
       [import-button "Load file" "*.txt" read-weighted-graph g]
       [d3/graph g node-types edge-types selected-id selected-edge-type callbacks]
       [:button {:on-click (fn solve-click [e] (shortest-path g))} "Solve"]
       [inspect @vis]])))
