(ns shortest-path-visual.view
  (:require
    [shortest-path-visual.force-directed :as d3]
    [shortest-path-visual.data :as data]
    [shortest-path-visual.algorithm :as algorithm]
    [shortest-path-visual.visualize :as visualize]
    [reagent.core :as reagent]
    [cljs.pprint :as pprint]))

(def node-types
  (reagent/atom {}))

(def edge-types
  (reagent/atom {}))

(def callbacks
  (reagent/atom {}))

(defn import-button [label accept deserialize g]
  [:li
   [:a.btn.btn-file
    label
    [:input
     {:type "file"
      :name "import"
      :tab-index "-1"
      :accept accept
      :value ""
      :on-change (fn import-changed [e] (data/import-graph e g deserialize))}]]])

(defn inspect [x]
  [:table
   {:style {:width "100%"
            :text-align "left"}}
   [:thead
    (into
      [:tr]
      (for [k (keys x)]
        [:th k]))]
   (into
     [:tr]
     (for [v (vals x)]
       [:td {:style {:vertical-align "top"
                     :width "20%"}}
        [:pre (with-out-str (pprint/pprint v))]]))])

(defn graph-loader []
  (let [g (reagent/atom {})
        selected-id (reagent/atom nil)
        selected-edge-type (reagent/atom nil)]
    (fn a-graph-loader []
      [:div
       [:button {:on-click (fn [e] (reset! g data/tiny))} "Example"]
       [import-button "Load file" "*.txt" data/read-weighted-graph g]
       [d3/graph g node-types edge-types selected-id selected-edge-type callbacks]
       [:button {:on-click (fn solve-click [e] (algorithm/shortest-path g))} "Solve"]
       [inspect @visualize/vis]])))
