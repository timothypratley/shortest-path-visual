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

(defn import-button [g]
  [:a.btn.btn-file
   "Load File (dot or txt): "
   [:input
    {:type "file"
     :name "import"
     :tab-index "-1"
     :accept ".dot,.txt"
     :value ""
     :on-change (fn import-changed [e] (data/import-graph e g))}]])

(defn inspect [x]
  [:table
   {:style {:width "100%"
            :text-align "left"}}
   [:thead
    (into
      [:tr]
      (for [k (keys x)]
        [:th k]))]
   [:tbody
    (into
      [:tr]
      (for [v (vals x)]
        [:td {:style {:vertical-align "top"
                      :width "20%"}}
         [:pre (with-out-str (pprint/pprint v))]]))]])

(defn shortest-path-solver []
  (reagent/with-let
    [g (reagent/atom {})
     start-node (reagent/atom nil)
     target-node (reagent/atom nil)
     selected-id (reagent/atom nil)
     selected-edge-type (reagent/atom nil)
     watch (add-watch
             selected-id
             :watch
             (fn [k r a b]
               (when b
                 (cond
                   (and @start-node @target-node)
                   (do (reset! start-node @target-node)
                       (reset! target-node b)
                       (algorithm/shortest-path g @start-node @target-node))
                   @start-node
                   (do (reset! target-node b)
                       (algorithm/shortest-path g @start-node @target-node))
                   :else (reset! start-node b)))))]
    [:div
     [:button {:on-click (fn [e] (reset! g data/tiny))} "Example"]
     [:div [import-button g]]
     [d3/graph g node-types edge-types selected-id selected-edge-type callbacks]
     [:div
      [:div "From: " (or @start-node "<click on a node>")]
      [:div "To: " (or @target-node "<click on a node>")]]
     [inspect @visualize/vis]]
    (finally
      (remove-watch selected-id :watch))))
